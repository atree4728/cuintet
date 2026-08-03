-- | 本の @alu.veryl@ に対応する。
module Cuintet.Alu where

import Clash.Prelude
import Cuintet.Corectrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (XLen)
import Cuintet.Util (fill)

alu :: InstCtrl -> BitVector XLen -> BitVector XLen -> BitVector XLen
alu InstCtrl{itype, isAluOp, funct3, funct7} op1 op2
  | not isAluOp = op1 + op2
  | funct3 == 0b000 && (itype == IType || funct7 == zeroBits) = op1 + op2
  | funct3 == 0b000 = op1 - op2
  | funct3 == 0b001 = sll
  | funct3 == 0b010 = slt
  | funct3 == 0b011 = sltu
  | funct3 == 0b100 = op1 `xor` op2
  | funct3 == 0b101 && testBit funct7 5 = sra
  | funct3 == 0b101 = srl
  | funct3 == 0b110 = op1 .|. op2
  | funct3 == 0b111 = op1 .&. op2
  | otherwise = deepErrorX "alu: unreachable path"
 where
  shiftWidth = bitCoerce $ zeroExtend (truncateB op2 :: BitVector 5)
  sll = op1 `shiftL` shiftWidth
  srl = op1 `shiftR` shiftWidth
  sra = bitCoerce $ (bitCoerce op1 :: Signed XLen) `shiftR` shiftWidth

  slt = unpack $ fill low ++# pack ((bitCoerce op1 :: Signed XLen) < (bitCoerce op2 :: Signed XLen))
  sltu = unpack $ fill low ++# pack (op1 < op2)
