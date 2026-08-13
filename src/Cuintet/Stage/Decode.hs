{- | ID: decodes the instruction, takes in the registers it reads, and decides whether to issue it.

The stage holds no state. An instruction it does not issue is simply left at the
head of the IF-ID FIFO and decoded again next clock, so neither a stall nor a
flush needs anything rolled back.
-}
module Cuintet.Stage.Decode (decode, DecodeIn (..), DecodeOut (..), immB, immJ) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..), usesRs1, usesRs2)
import Cuintet.Eei (Inst, MulDivType, Opcode (..), RegAddr, System12 (..), SystemOp (..), XLen)
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
    (ctrl, imm) = instDecode instBits
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

    idEx = IdEx {..}
    issued = isJust entry && not (any (hazard idEx) pending) && wready && not flush

immB :: Inst -> BitVector XLen
immB instBits = signExtend (immBG ++# (0 :: BitVector 1))
  where
    immBG = slice d31 d31 instBits ++# slice d7 d7 instBits ++# slice d30 d25 instBits ++# slice d11 d8 instBits

immJ :: Inst -> BitVector XLen
immJ instBits = signExtend (immJG ++# (0 :: BitVector 1))
  where
    immJG = slice d31 d31 instBits ++# slice d19 d12 instBits ++# slice d20 d20 instBits ++# slice d30 d21 instBits

-- | The control flags and the immediate, both a function of the opcode alone.
instDecode :: Inst -> (InstCtrl, BitVector XLen)
instDecode instBits = (ctrl op, imm op)
  where
    op = unpack $ slice d6 d0 instBits

    immIG = slice d31 d20 instBits
    immSG = slice d31 d25 instBits ++# slice d11 d7 instBits
    immUG = slice d31 d12 instBits

    immI = signExtend immIG
    immS = signExtend immSG
    immU = signExtend (immUG ++# (0 :: BitVector 12))

    imm :: Opcode -> BitVector XLen
    imm LUI = immU
    imm AUIPC = immU
    imm OP_IMM = immI
    imm OP_IMM_32 = immI
    imm JAL = immJ instBits
    imm JALR = immI
    imm BRANCH = immB instBits
    imm LOAD = immI
    imm STORE = immS
    imm MISC_MEM = immI -- @fm@, @pred@ and @succ@ sit in the I-immediate field
    imm SYSTEM = immI -- use [11:0]
    imm _ = deepErrorX "imm: opcode carries no immediate"

    funct3 = slice d14 d12 instBits
    funct7 = slice d31 d25 instBits

    systemOp :: Maybe SystemOp
    systemOp = case op of
      SYSTEM
        | funct3 /= 0 -> Just $ SysCsr (unpack funct3)
        | ECALL <- System12 immIG -> Just SysEcall
        | MRET <- System12 immIG -> Just SysMret
        | otherwise -> Just SysIllegal
      _ -> Nothing

    mulDiv :: Maybe MulDivType
    mulDiv = case op of
      OP_REG -> extM
      OP_REG_32 -> extM
      _ -> Nothing
      where
        extM = orNothing (funct7 == 1) (unpack funct3)

    instCtrl itype rwbEn isLui isAluOp isOp32 isJump isLoad = InstCtrl {..}

    ctrl :: Opcode -> InstCtrl
{- FOURMOLU_DISABLE -}
    ctrl LUI       = instCtrl UType  True  True False False False False
    ctrl AUIPC     = instCtrl UType  True False False False False False
    ctrl JAL       = instCtrl JType  True False False False  True False
    ctrl JALR      = instCtrl IType  True False False False  True False
    ctrl BRANCH    = instCtrl BType False False False False False False
    ctrl LOAD      = instCtrl IType  True False False False False  True
    ctrl STORE     = instCtrl SType False False False False False False
    ctrl OP_IMM    = instCtrl IType  True False  True False False False
    ctrl OP_REG    = instCtrl RType  True False  True False False False
    ctrl OP_IMM_32 = instCtrl IType  True False  True  True False False
    ctrl OP_REG_32 = instCtrl RType  True False  True  True False False
    ctrl MISC_MEM  = instCtrl IType False False False False False False -- fench is nop
    ctrl SYSTEM    = instCtrl IType  True False False False False False
    ctrl _         = deepErrorX "ctrl: unknown opcode"
{- FOURMOLU_ENABLE -}

{- | Whether a source register of this instruction is still to be written by an
instruction downstream, given as 'Cuintet.Pipeline.unresolved' of that stage.
-}
hazard :: IdEx -> Maybe RegAddr -> Bool
hazard IdEx {ctrl, rs1Addr, rs2Addr} = maybe False $
  \rd -> usesRs1 ctrl && rd == rs1Addr || usesRs2 ctrl && rd == rs2Addr
