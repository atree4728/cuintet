{- | ID: decodes the instruction, takes in the registers it reads, and decides whether to issue it.

The stage holds no state. An instruction it does not issue is simply left at the
head of the IF-ID FIFO and decoded again next clock, so neither a stall nor a
flush needs anything rolled back.
-}
module Cuintet.Stage.Decode (decode, DecodeIn (..), DecodeOut (..), immI, immS, immB, immU, immJ) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..), usesRs1, usesRs2)
import Cuintet.Eei (AccessWidth (..), BranchCond (..), CsrOp (..), IOp (..), Inst, MulDivType (..), MulOp (..), Opcode (..), RegAddr, Sign (..), System12 (..), SystemOp (..), XLen, pattern BREAKPOINT, pattern ENVIRONMENT_CALL_FROM_M_MODE, pattern ILLEGAL_INSTRUCTION)
import Cuintet.Pipeline (IdEx (..), IfId (..), srcRegs)
import Cuintet.Unit.RegFile (RegResp (..))
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

data DecodeIn = DecodeIn
  { entry :: Maybe IfId
  -- ^ The instruction at the head of the IF-ID FIFO.
  , regResp :: RegResp
  -- ^ The operands, read out of 'Cuintet.RegFile.regFile' for that same entry.
  , forwards :: Vec 2 (Maybe (RegAddr, BitVector XLen))
  , pending :: Vec 2 (Maybe RegAddr)
  -- ^ What each stage downstream will write back but cannot forward yet.
  , wready :: Bool
  -- ^ Whether the ID-EX FIFO can accept a write.
  , flush :: Bool
  -- ^ Whether MA is redirecting IF this clock.
  }

-- | The instruction handed to EX, absent on a clock ID does not issue.
newtype DecodeOut = DecodeOut {issue :: Maybe IdEx}

-- | One clock of ID.
decode :: DecodeIn -> DecodeOut
decode DecodeIn {..} = DecodeOut {issue = orNothing issued idEx}
  where
    IfId {..} = fromMaybe (deepErrorX "decode: IF-ID FIFO is empty") entry
    (ctrl, imm, legal) = instDecode instBits
    (rs1Addr, rs2Addr) = srcRegs instBits
    rdAddr = slice d11 d7 instBits
    RegResp {rs1Data = rs1Read, rs2Data = rs2Read} = regResp
    rs1Data = resolveForwarding (usesRs1 ctrl) rs1Addr rs1Read
    rs2Data = resolveForwarding (usesRs2 ctrl) rs2Addr rs2Read

    resolveForwarding uses rs old
      | not uses = old
      | otherwise = fromMaybe old $ foldr ((<|>) . (>>= match)) Nothing forwards
      where
        match (rd, d) = orNothing (rd == rs) d

    exception
      | not legal = Just (ILLEGAL_INSTRUCTION, zeroExtend instBits)
      | Just SysEcall <- ctrl.systemOp = Just (ENVIRONMENT_CALL_FROM_M_MODE, 0)
      | Just SysEbreak <- ctrl.systemOp = Just (BREAKPOINT, pack pc)
      | otherwise = Nothing

    idEx = IdEx {..}
    issued = isJust entry && not (any (hazard idEx) pending) && wready && not flush
{-# OPAQUE decode #-}

immI, immS, immB, immU, immJ :: Inst -> BitVector XLen
immI instBits = signExtend $ slice d31 d20 instBits
immS instBits = signExtend $ slice d31 d25 instBits ++# slice d11 d7 instBits
immB instBits = signExtend $ slice d31 d31 instBits ++# slice d7 d7 instBits ++# slice d30 d25 instBits ++# slice d11 d8 instBits ++# (0 :: BitVector 1)
immU instBits = signExtend $ slice d31 d12 instBits ++# (0 :: BitVector 12)
immJ instBits = signExtend $ slice d31 d31 instBits ++# slice d19 d12 instBits ++# slice d20 d20 instBits ++# slice d30 d21 instBits ++# (0 :: BitVector 1)

-- | The control flags and the immediate, both a function of the opcode alone.
instDecode :: Inst -> (InstCtrl, BitVector XLen, Bool)
instDecode instBits = case op of
  LUI -> (instCtrl UType True True False False False False, immU instBits, True)
  AUIPC -> (instCtrl UType True False False False False False, immU instBits, True)
  JAL -> (instCtrl JType True False False False True False, immJ instBits, True)
  JALR -> (instCtrl IType True False False False True False, immI instBits, True)
  BRANCH -> (instCtrl BType False False False False False False, immB instBits, legalBranch)
  LOAD -> (instCtrl IType True False False False False True, immI instBits, legalLoad)
  STORE -> (instCtrl SType False False False False False False, immS instBits, legalStore)
  OP_IMM -> (instCtrl IType True False True False False False, immI instBits, legalOpImm)
  OP_REG -> (instCtrl RType True False True False False False, noImm, legalOpReg)
  OP_IMM_32 -> (instCtrl IType True False True True False False, immI instBits, legalOpImm32)
  OP_REG_32 -> (instCtrl RType True False True True False False, noImm, legalOpReg32)
  MISC_MEM -> (instCtrl IType False False False False False False, immI instBits, True)
  SYSTEM -> (instCtrl IType True False False False False False, immI instBits, legalSystem)
  _ -> (instCtrl IType False False False False False False, noImm, False)
  where
    op = unpack $ slice d6 d0 instBits
    funct3 = slice d14 d12 instBits
    funct7 = slice d31 d25 instBits

    instCtrl itype rwbEn isLui isAluOp isOp32 isJump isLoad = InstCtrl {..}

    noImm = deepErrorX "instDecode: opcode carries no immediate"

    legalBranch = case unpack funct3 :: BranchCond of
      BranchIllegal -> False
      _ -> True

    legalLoad = case unpack funct3 :: AccessWidth of
      WidthIllegal -> False
      _ -> True

    legalStore = case unpack funct3 :: AccessWidth of
      Byte Signed -> True
      Half Signed -> True
      Word Signed -> True
      DoubleWord -> True
      _ -> False

    legalOpImm = case unpack funct3 :: IOp of
      SLL -> f7Hi == 0b000000 -- SLLI
      SR -> f7Hi == 0b000000 || f7Hi == 0b010000 -- SRLI, SRAI
      _ -> True
      where
        f7Hi = slice d6 d1 funct7

    legalOpReg = case funct7 of
      0b0000000 -> True -- RV32I
      0b0100000 -> case unpack funct3 :: IOp of
        ADD -> True -- SUB
        SR -> True -- SRA
        _ -> False
      0b0000001 -> True -- RV32M
      _ -> False

    legalOpImm32 = case unpack funct3 :: IOp of
      ADD -> True -- ADDIW
      SLL -> funct7 == 0b0000000 -- SLLIW
      SR -> funct7 == 0b0000000 || funct7 == 0b0100000 -- SRLIW, SRAIW
      _ -> False

    legalOpReg32 = case funct7 of
      0b0000001 -> case unpack funct3 :: MulDivType of
        Multiply MulLow -> True -- MULW
        Division _ -> True -- DIVW, DIVUW, REMW, REMUW
        _ -> False
      0b0000000 -> case unpack funct3 :: IOp of
        ADD -> True -- ADDW
        SLL -> True -- SLLW
        SR -> True -- SRLW
        _ -> False
      0b0100000 -> case unpack funct3 :: IOp of
        ADD -> True -- SUBW
        SR -> True -- SRAW
        _ -> False
      _ -> False

    legalSystem = case systemOp of
      Just SysIllegal -> False
      Just (SysCsr (_, CsrIllegal)) -> False
      _ -> True

    systemOp :: Maybe SystemOp
    systemOp = case op of
      SYSTEM
        | funct3 /= 0 -> Just $ SysCsr (unpack funct3)
        | ECALL <- system12 -> Just SysEcall
        | EBREAK <- system12 -> Just SysEbreak
        | MRET <- system12 -> Just SysMret
        | otherwise -> Just SysIllegal
      _ -> Nothing
      where
        system12 = System12 $ slice d31 d20 instBits

    mulDiv :: Maybe MulDivType
    mulDiv = case op of
      OP_REG -> extM
      OP_REG_32 -> extM
      _ -> Nothing
      where
        extM = orNothing (funct7 == 1) (unpack funct3)

{- | Whether a source register of this instruction is still to be written by an
instruction downstream, given as 'Cuintet.Pipeline.unresolved' of that stage.
-}
hazard :: IdEx -> Maybe RegAddr -> Bool
hazard IdEx {ctrl, rs1Addr, rs2Addr} = maybe False
  $ \rd -> usesRs1 ctrl && rd == rs1Addr || usesRs2 ctrl && rd == rs2Addr
