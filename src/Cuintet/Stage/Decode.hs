-- | ID: decodes the instruction, takes in the registers it reads, and decides whether to issue it.
module Cuintet.Stage.Decode (decode, DecodeIn (..), DecodeOut (..), immI, immS, immB, immU, immJ) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstFormat (..), usesRs1, usesRs2)
import Cuintet.Eei (AluOp, Inst, MemOp (..), Opcode (..), RegAddr, System12 (..), SystemOp (..), XLen, parseBranchOp, parseCsr, parseLoad, parseStore, pattern BREAKPOINT, pattern ENVIRONMENT_CALL_FROM_M_MODE, pattern ILLEGAL_INSTRUCTION)
import Cuintet.Pipeline (IdEx (..), IfId (..), srcRegs)
import Cuintet.Unit.RegFile (RegResp (..))
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust, isNothing)

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
    decoded = instDecode instBits
    (ctrl, imm) = fromMaybe (trapCtrl, 0) decoded
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
      | isNothing decoded = Just (ILLEGAL_INSTRUCTION, zeroExtend instBits)
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

-- | The control flags and the immediate. 'Nothing' when the bits name no instruction the implementation has; that is what raises @ILLEGAL_INSTRUCTION@.
instDecode :: Inst -> Maybe (InstCtrl, BitVector XLen)
instDecode instBits = case opcode instBits of
  LUI -> Just (uType {rwbEn = True, isLui = True}, immU instBits)
  AUIPC -> Just (uType {rwbEn = True}, immU instBits)
  JAL -> Just (jType {rwbEn = True, isJump = True}, immJ instBits)
  JALR -> orNothing (f3 == 0) (iType {rwbEn = True, isJump = True}, immI instBits)
  BRANCH -> do
    cond <- parseBranchOp f3
    Just (bType {branchOp = Just cond}, immB instBits)
  LOAD -> do
    (width, sign) <- parseLoad f3
    Just (iType {rwbEn = True, memOp = Just (Load width sign)}, immI instBits)
  STORE -> do
    width <- parseStore f3
    Just (sType {memOp = Just (Store width)}, immS instBits)
  OP_IMM -> do
    op <- parseOpImm instBits
    Just (iType {rwbEn = True, aluOp = Just op}, immI instBits)
  OP_REG -> case funct7 instBits of
    0b0000000 -> Just (opReg 0)
    0b0100000 -> orNothing (f3 == 0b000 || f3 == 0b101) (opReg 1) -- SUB, SRA
    0b0000001 -> Just (rType {rwbEn = True, mulDivOp = Just (unpack f3)}, noImm) -- M
    _ -> Nothing
  OP_IMM_32 -> do
    op <- parseOpImm32 instBits
    Just (iType {rwbEn = True, aluOp = Just op, isOp32 = True}, immI instBits)
  OP_REG_32 -> case funct7 instBits of
    0b0000000 -> orNothing (f3 == 0b000 || f3 == 0b001 || f3 == 0b101) (opReg32 0) -- ADDW, SLLW, SRLW
    0b0100000 -> orNothing (f3 == 0b000 || f3 == 0b101) (opReg32 1) -- SUBW, SRAW
    0b0000001 -> orNothing (f3 == 0b000 || msb f3 == 1) (rType {rwbEn = True, mulDivOp = Just (unpack f3), isOp32 = True}, noImm) -- MULW, DIVW, DIVUW, REMW, REMUW
    _ -> Nothing
  -- FENCE orders nothing this core reorders, so it is a nop; FENCE.I is Zifencei, which it does not have
  MISC_MEM -> orNothing (f3 == 0 && noRegs instBits) (iType, immI instBits)
  SYSTEM -> do
    op <- parseSystem instBits
    Just (iType {rwbEn = True, systemOp = Just op}, immI instBits)
  _ -> Nothing
  where
    f3 = funct3 instBits
    opReg alt = (rType {rwbEn = True, aluOp = Just (aluOpOf f3 alt)}, noImm)
    opReg32 alt = (rType {rwbEn = True, aluOp = Just (aluOpOf f3 alt), isOp32 = True}, noImm)
    rType = blank RType
    iType = blank IType
    sType = blank SType
    bType = blank BType
    uType = blank UType
    jType = blank JType
    noImm = 0 -- the opcode carries no immediate; kept zero so nothing downstream sees an X

-- | An 'InstCtrl' of the given form that does nothing at all; what every arm of 'instDecode' starts from.
blank :: InstFormat -> InstCtrl
blank format = InstCtrl {format, rwbEn = False, isLui = False, aluOp = Nothing, isOp32 = False, isJump = False, memOp = Nothing, branchOp = Nothing, mulDivOp = Nothing, systemOp = Nothing}

trapCtrl :: InstCtrl
trapCtrl = blank IType

opcode :: Inst -> Opcode
opcode = unpack . slice d6 d0

funct3 :: Inst -> BitVector 3
funct3 = slice d14 d12

funct7 :: Inst -> BitVector 7
funct7 = slice d31 d25

funct12 :: Inst -> BitVector 12
funct12 = slice d31 d20

-- | Whether @rs1@ and @rd@ are both zero, as the forms that name neither require.
noRegs :: Inst -> Bool
noRegs instBits = slice d19 d15 instBits == 0 && slice d11 d7 instBits == 0

-- | The four bits an 'AluOp' is: @funct3@ over the bit that picks the subtracting and arithmetic forms.
aluOpOf :: BitVector 3 -> BitVector 1 -> AluOp
aluOpOf f3 alt = unpack (f3 ++# alt)

-- | The ALU operation an @OP-IMM@ instruction names.
parseOpImm :: Inst -> Maybe AluOp
parseOpImm instBits = case funct3 instBits of
  0b001 -> orNothing (f7Hi == 0b000000) shiftOp -- SLLI
  0b101 -> orNothing (f7Hi == 0b000000 || f7Hi == 0b010000) shiftOp -- SRLI, SRAI
  f3 -> Just (aluOpOf f3 0)
  where
    f7Hi = slice d31 d26 instBits
    shiftOp = aluOpOf (funct3 instBits) (slice d30 d30 instBits)

-- | The ALU operation an @OP-IMM-32@ instruction names.
parseOpImm32 :: Inst -> Maybe AluOp
parseOpImm32 instBits = case funct3 instBits of
  0b000 -> Just (aluOpOf 0b000 0) -- ADDIW
  0b001 -> orNothing (f7 == 0b0000000) (aluOpOf 0b001 0) -- SLLIW
  0b101 -> orNothing (f7 == 0b0000000 || f7 == 0b0100000) (aluOpOf 0b101 (slice d30 d30 instBits)) -- SRLIW, SRAIW
  _ -> Nothing
  where
    f7 = funct7 instBits

-- | What a @SYSTEM@ instruction asks of the execution environment.
parseSystem :: Inst -> Maybe SystemOp
parseSystem instBits
  | funct3 instBits /= 0 = SysCsr <$> parseCsr (funct3 instBits) (funct12 instBits)
  | not (noRegs instBits) = Nothing
  | ECALL <- System12 f12 = Just SysEcall
  | EBREAK <- System12 f12 = Just SysEbreak
  | MRET <- System12 f12 = Just SysMret
  | otherwise = Nothing
  where
    f12 = funct12 instBits

{- | Whether a source register of this instruction is still to be written by an
instruction downstream, given as 'Cuintet.Pipeline.unresolved' of that stage.
-}
hazard :: IdEx -> Maybe RegAddr -> Bool
hazard IdEx {ctrl, rs1Addr, rs2Addr} = maybe False
  $ \rd -> usesRs1 ctrl && rd == rs1Addr || usesRs2 ctrl && rd == rs2Addr
