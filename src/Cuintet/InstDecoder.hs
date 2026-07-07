module Cuintet.InstDecoder where

import Clash.Prelude
import Cuintet.Corectrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (Inst, OpCode (..), XLen, opDecode)

instDecode :: Inst -> (InstCtrl, BitVector XLen)
instDecode bits = (ctrl op, imm op)
 where
  op = opDecode $ slice d6 d0 bits

  fill :: forall n. (KnownNat n) => Bit -> BitVector n
  fill b = pack $ replicate (SNat :: SNat n) b

  immIG = slice d31 d20 bits
  immSG = slice d31 d25 bits ++# slice d11 d7 bits
  immBG = slice d31 d31 bits ++# slice d7 d7 bits ++# slice d30 d25 bits ++# slice d11 d8 bits
  immUG = slice d31 d12 bits
  immJG = slice d31 d31 bits ++# slice d19 d12 bits ++# slice d20 d20 bits ++# slice d30 d21 bits

  --
  immI = fill (msb bits) ++# immIG
  immS = fill (msb bits) ++# immSG
  immB = fill (msb bits) ++# immBG ++# (1 :: BitVector 1)
  immU = fill (msb bits) ++# immUG ++# (0 :: BitVector 12)
  immJ = fill (msb bits) ++# immJG

  imm :: OpCode -> BitVector XLen
  imm Lui = immU
  imm AuiPc = immU
  imm Op = deepErrorX "imm: Op"
  imm OpImm = immI
  imm Jal = immJ
  imm Jalr = immI
  imm Branch = immB
  imm Load = immI
  imm Store = immS

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

  ctrl :: OpCode -> InstCtrl
{- ORMOLU_DISABLE -}
  ctrl Lui    = instCtrl UType  True  True False False False
  ctrl AuiPc  = instCtrl UType  True False False False False
  ctrl Jal    = instCtrl JType  True False False  True False
  ctrl Jalr   = instCtrl IType  True False False  True False
  ctrl Branch = instCtrl BType False False False False False
  ctrl Load   = instCtrl IType  True False False False  True
  ctrl Store  = instCtrl SType False False False False False
  ctrl Op     = instCtrl RType  True False  True False False
  ctrl OpImm  = instCtrl IType  True False  True False False
{- ORMOLU_ENABLE -}
