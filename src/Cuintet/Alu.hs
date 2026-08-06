module Cuintet.Alu (alu) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (IOp (..), ShiftRight (..), XLen)

{- | The ALU.  A @*W@ instruction is its plain counterpart narrowed to 32 bits
and sign-extended back, so 'exec' is width-polymorphic and @isOp32@ is decided
in exactly one place.
-}
alu :: InstCtrl -> BitVector XLen -> BitVector XLen -> BitVector XLen
alu InstCtrl {itype, isAluOp, isOp32, funct3, funct7} op1 op2
  | not isAluOp = op1 + op2
  | isOp32 = signExtend $ exec shamt32 (truncateB op1 :: BitVector 32) (truncateB op2)
  | otherwise = exec shamt64 op1 op2
  where
    shamt64 = unpack $ zeroExtend (truncateB op2 :: BitVector 6)
    shamt32 = unpack $ zeroExtend (truncateB op2 :: BitVector 5)

    isSub = itype /= IType && funct7 /= zeroBits

    exec :: forall n' n. (KnownNat n', n ~ n' + 1) => Int -> BitVector n -> BitVector n -> BitVector n
    exec shamt a b = case unpack funct3 of
      ADD | isSub -> a - b
      ADD -> a + b
      SLL -> a `shiftL` shamt
      SLT -> boolToBV $ signed a < signed b
      SLTU -> boolToBV $ a < b
      XOR -> a `xor` b
      SR -> case unpack (slice d5 d5 funct7) of
        Logical -> a `shiftR` shamt
        Arithmetic -> pack $ signed a `shiftR` shamt
      OR -> a .|. b
      AND -> a .&. b
      where
        signed x = bitCoerce x :: Signed n
