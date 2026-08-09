module Cuintet.Stage.Execute (execute, ExecuteIn (..), ExecuteOut (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (Addr, IOp (..), ShiftRight (..), XLen)
import Cuintet.Pipeline (ExMa (..), IdEx (..))
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

data ExecuteIn = ExecuteIn
  { entry :: Maybe IdEx
  , wready :: Bool
  }

newtype ExecuteOut = ExecuteOut
  { issue :: Maybe ExMa
  }

execute :: ExecuteIn -> ExecuteOut
execute ExecuteIn {..} = ExecuteOut {issue = orNothing issued exMa}
  where
    issued = isJust entry && wready
    exMa = mkExMa $ fromMaybe (deepErrorX "coreT: ID-EX FIFO is empty") entry

mkExMa :: IdEx -> ExMa
mkExMa IdEx {..} = ExMa {op1, op2, aluResult, branchTaken, ..}
  where
    (op1, op2) = operands ctrl imm rs1Data rs2Data pc
    aluResult = alu ctrl op1 op2

    branchTaken = branchUnit ctrl.funct3 op1 op2

    wbData
      | ctrl.isLui = imm
      | ctrl.isJump = bitCoerce (pc + 4)
      | otherwise = aluResult

-- | Extract the two operands according to the instruction form.
operands ::
  InstCtrl ->
  BitVector XLen ->
  BitVector XLen ->
  BitVector XLen ->
  Addr ->
  (BitVector XLen, BitVector XLen)
operands InstCtrl {itype} imm rs1Data rs2Data pc = case itype of
  RType -> (rs1Data, rs2Data)
  BType -> (rs1Data, rs2Data)
  IType -> (rs1Data, imm)
  SType -> (rs1Data, imm)
  UType -> (bitCoerce pc, imm)
  JType -> (bitCoerce pc, imm)

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

branchUnit :: BitVector 3 -> BitVector XLen -> BitVector XLen -> Bool
branchUnit funct3 op1 op2 = case funct3 of
  0b000 -> beq
  0b001 -> not beq
  0b100 -> blt
  0b101 -> not blt
  0b110 -> bltu
  0b111 -> not bltu
  _ -> False
  where
    beq = op1 == op2
    blt = (bitCoerce op1 :: Signed XLen) < (bitCoerce op2 :: Signed XLen)
    bltu = (bitCoerce op1 :: Unsigned XLen) < (bitCoerce op2 :: Unsigned XLen)
