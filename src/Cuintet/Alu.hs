module Cuintet.Alu (alu) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (IOp (..), ShiftRight (..), XLen)

alu :: InstCtrl -> BitVector XLen -> BitVector XLen -> BitVector XLen
alu InstCtrl {itype, isAluOp, isOp32, funct3, funct7} op1 op2
  | not isAluOp = op1 + op2
  | otherwise = case unpack funct3 of
      ADD | isOp32 -> signExtend $ if isSub then op1lw - op2lw else op1lw + op2lw
      ADD -> if isSub then op1 - op2 else op1 + op2
      SLL -> sll
      SLT -> slt
      SLTU -> sltu
      XOR -> op1 `xor` op2
      SR -> case unpack (slice d5 d5 funct7) of
        Logical -> srl
        Arithmetic -> sra
      OR -> op1 .|. op2
      AND -> op1 .&. op2
  where
    isSub = itype /= IType && funct7 /= zeroBits
    op1lw = truncateB op1 :: BitVector 32
    op2lw = truncateB op2 :: BitVector 32

    shiftWidth = unpack $ zeroExtend (truncateB op2 :: BitVector 6)
    sll = op1 `shiftL` shiftWidth
    srl = op1 `shiftR` shiftWidth
    sra = bitCoerce $ (bitCoerce op1 :: Signed XLen) `shiftR` shiftWidth

    slt = boolToBV ((bitCoerce op1 :: Signed XLen) < (bitCoerce op2 :: Signed XLen))
    sltu = boolToBV (op1 < op2)
