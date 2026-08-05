module Cuintet.InstDecoder (instDecode) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (Inst, Opcode (..), XLen)

instDecode :: Inst -> (InstCtrl, BitVector XLen)
instDecode bits = (ctrl op, imm op)
  where
    op = unpack $ slice d6 d0 bits

    immIG = slice d31 d20 bits
    immSG = slice d31 d25 bits ++# slice d11 d7 bits
    immBG = slice d31 d31 bits ++# slice d7 d7 bits ++# slice d30 d25 bits ++# slice d11 d8 bits
    immUG = slice d31 d12 bits
    immJG = slice d31 d31 bits ++# slice d19 d12 bits ++# slice d20 d20 bits ++# slice d30 d21 bits

    immI = signExtend immIG
    immS = signExtend immSG
    immB = signExtend (immBG ++# (0 :: BitVector 1))
    immU = signExtend (immUG ++# (0 :: BitVector 12))
    immJ = signExtend (immJG ++# (0 :: BitVector 1))

    imm :: Opcode -> BitVector XLen
    imm LUI = immU
    imm AUIPC = immU
    imm OP_IMM = immI
    imm JAL = immJ
    imm JALR = immI
    imm BRANCH = immB
    imm LOAD = immI
    imm STORE = immS
    imm _ = deepErrorX "imm: opcode carries no immediate"

    instCtrl itype rwbEn isLui isAluOp isJump isLoad =
      InstCtrl
        { itype
        , rwbEn
        , isLui
        , isAluOp
        , isJump
        , isLoad
        , funct3 = slice d14 d12 bits
        , funct7 = slice d31 d25 bits
        }

    ctrl :: Opcode -> InstCtrl
{- FOURMOLU_DISABLE -}
    ctrl LUI    = instCtrl UType  True  True False False False
    ctrl AUIPC  = instCtrl UType  True False False False False
    ctrl JAL    = instCtrl JType  True False False  True False
    ctrl JALR   = instCtrl IType  True False False  True False
    ctrl BRANCH = instCtrl BType False False False False False
    ctrl LOAD   = instCtrl IType  True False False False  True
    ctrl STORE  = instCtrl SType False False False False False
    ctrl OP     = instCtrl RType  True False  True False False
    ctrl OP_IMM = instCtrl IType  True False  True False False
    ctrl _      = deepErrorX "ctrl: unknown opcode"
{- FOURMOLU_ENABLE -}
