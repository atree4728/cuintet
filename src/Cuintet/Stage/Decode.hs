module Cuintet.Stage.Decode (decode, DecodeIn (..), DecodeOut (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (Inst, Opcode (..), RegAddr, RegFile, System12 (..), SystemOp (..), XLen)
import Cuintet.Pipeline (IdEx (..), IfId (..))
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

data DecodeIn = DecodeIn
  { entry :: Maybe IfId
  , regFile :: RegFile
  , inflights :: Vec 3 (Maybe RegAddr)
  , wready :: Bool
  , flush :: Bool
  }

newtype DecodeOut = DecodeOut {issue :: Maybe IdEx}

decode :: DecodeIn -> DecodeOut
decode DecodeIn {..} = DecodeOut {issue = orNothing issued idEx}
  where
    idEx = mkIdEx regFile $ fromMaybe (deepErrorX "coreT: IF-ID FIFO is empty") entry
    issued = isJust entry && not (hazard idEx inflights) && wready && not flush

mkIdEx :: Vec 32 (BitVector XLen) -> IfId -> IdEx
mkIdEx regFile IfId {pc, instBits} = IdEx {pc, instBits, ctrl, imm, rs1Addr, rs2Addr, rdAddr, rs1Data, rs2Data}
  where
    (ctrl, imm) = instDecode instBits
    rs1Addr = slice d19 d15 instBits
    rs2Addr = slice d24 d20 instBits
    rdAddr = slice d11 d7 instBits
    rs1Data = regFile !! rs1Addr
    rs2Data = regFile !! rs2Addr

instDecode :: Inst -> (InstCtrl, BitVector XLen)
instDecode instBits = (ctrl op, imm op)
  where
    op = unpack $ slice d6 d0 instBits

    immIG = slice d31 d20 instBits
    immSG = slice d31 d25 instBits ++# slice d11 d7 instBits
    immBG = slice d31 d31 instBits ++# slice d7 d7 instBits ++# slice d30 d25 instBits ++# slice d11 d8 instBits
    immUG = slice d31 d12 instBits
    immJG = slice d31 d31 instBits ++# slice d19 d12 instBits ++# slice d20 d20 instBits ++# slice d30 d21 instBits

    immI = signExtend immIG
    immS = signExtend immSG
    immB = signExtend (immBG ++# (0 :: BitVector 1))
    immU = signExtend (immUG ++# (0 :: BitVector 12))
    immJ = signExtend (immJG ++# (0 :: BitVector 1))

    imm :: Opcode -> BitVector XLen
    imm LUI = immU
    imm AUIPC = immU
    imm OP_IMM = immI
    imm OP_IMM_32 = immI
    imm JAL = immJ
    imm JALR = immI
    imm BRANCH = immB
    imm LOAD = immI
    imm STORE = immS
    imm MISC_MEM = immI -- @fm@, @pred@ and @succ@ sit in the I-immediate field
    imm SYSTEM = immI -- use [11:0]
    imm _ = deepErrorX "imm: opcode carries no immediate"

    funct3 = slice d14 d12 instBits
    funct7 = slice d31 d25 instBits

    -- @funct3@ tells a CSR access from the rest; @immIG@ /is/ the @funct12@ field.
    systemOp :: Opcode -> Maybe SystemOp
    systemOp SYSTEM
      | funct3 /= 0 = Just $ SysCsr (unpack funct3)
      | ECALL <- System12 immIG = Just SysEcall
      | MRET <- System12 immIG = Just SysMret
      | otherwise = Just SysIllegal
    systemOp _ = Nothing

    instCtrl itype rwbEn isLui isAluOp isOp32 isJump isLoad =
      InstCtrl
        { itype
        , rwbEn
        , isLui
        , isAluOp
        , isOp32
        , isJump
        , isLoad
        , systemOp = systemOp op
        , funct3
        , funct7
        }

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

usesRs1, usesRs2 :: InstCtrl -> Bool
usesRs1 InstCtrl {itype} = case itype of
  UType -> False
  JType -> False
  _ -> True
usesRs2 InstCtrl {itype} = case itype of
  RType -> True
  SType -> True
  BType -> True
  _ -> False

hazard :: (KnownNat n) => IdEx -> Vec n (Maybe RegAddr) -> Bool
hazard IdEx {ctrl, rs1Addr, rs2Addr} = any conflicts
  where
    conflicts = maybe False $
      \rd ->
        usesRs1 ctrl && rd == rs1Addr
          || usesRs2 ctrl && rd == rs2Addr
