-- | EX: operand selection, the ALU, the branch condition, and the multiply\/divide unit.
module Cuintet.Stage.Execute (execute, ExecuteIn (..), ExecuteOut (..)) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), InstFormat (..), isBranchOp)
import Cuintet.Eei (Addr, AluOp (..), BranchOp (..), SystemOp (..), XLen, misalignedCause, pattern INSTRUCTION_ADDRESS_MISALIGNED)
import Cuintet.Pipeline (ExMa (..), IdEx (..))
import Cuintet.Unit.Btb (BtbWrite, predicted, train)
import Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState, mkMulDivJob, mulDivStep)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust, isNothing)

data ExecuteIn = ExecuteIn
  { entry :: Maybe IdEx
  -- ^ The instruction at the head of the ID-EX FIFO.
  , wready :: Bool
  -- ^ Whether the EX-MA FIFO can accept a write.
  , serializingInFlight :: Bool
  }

data ExecuteOut = ExecuteOut
  { issue :: Maybe ExMa
  -- ^ The instruction handed to MA.
  , redirect :: Maybe Addr
  , btbWrite :: Maybe BtbWrite
  }

-- | One clock of EX. A multiply or divide sits here for several of them; it leaves on the one the unit produces its result.
execute :: MulDivState -> ExecuteIn -> (MulDivState, ExecuteOut)
execute mulDivState ExecuteIn {..} = (mulDivState', exOut)
  where
    (mulDivState', mulDivResp) = mulDivStep mulDivState MulDivReq {job = mkMulDivJob =<< entry, wready = wready && not serializingInFlight}

    issued = isJust entry && wready && not serializingInFlight && not mulDivResp.stall
    IdEx {..} = fromMaybe (deepErrorX "execute: ID-EX FIFO is empty") entry
    exOut = ExecuteOut {issue = orNothing issued ExMa {exception = exception', ..}, ..}

    (op1, op2) = operands ctrl imm rs1Data rs2Data pc
    aluResult = alu ctrl op1 op2
    branchTaken = maybe False (\cond -> branchUnit cond op1 op2) ctrl.branchOp

    wbData
      | isJust ctrl.mulDivOp = fromMaybe (deepErrorX "execute: muldiv committed without a result") mulDivResp.result
      | ctrl.isLui = imm
      | ctrl.isJump = bitCoerce (pc + 4)
      | otherwise = aluResult

    nextPc
      | ctrl.isJump = unpack (aluResult .&. complement 1)
      | isBranchOp ctrl && branchTaken = pc + numConvert imm
      | otherwise = pc + 4

    targetException =
      orNothing
        ((truncateB (pack nextPc) :: BitVector 2) /= 0)
        (INSTRUCTION_ADDRESS_MISALIGNED, pack nextPc)

    accessException = do
      memOp <- ctrl.memOp
      cause <- misalignedCause memOp (unpack aluResult)
      pure (cause, aluResult)

    exception' = exception <|> targetException <|> accessException

    isSerial = isJust exception' || ctrl.systemOp == Just SysMret

    redirect = orNothing (issued && not isSerial && nextPc /= predicted pc prediction) nextPc

    btbWrite = guard (issued && isNothing exception') >> train pc prediction (orNothing (nextPc /= pc + 4) nextPc)
{-# OPAQUE execute #-}

-- | Extract the two operands according to the instruction form.
operands ::
  InstCtrl ->
  BitVector XLen ->
  BitVector XLen ->
  BitVector XLen ->
  Addr ->
  (BitVector XLen, BitVector XLen)
operands InstCtrl {format} imm rs1Data rs2Data pc = case format of
  RType -> (rs1Data, rs2Data)
  BType -> (rs1Data, rs2Data)
  IType -> (rs1Data, imm)
  SType -> (rs1Data, imm)
  UType -> (bitCoerce pc, imm)
  JType -> (bitCoerce pc, imm)

-- | The ALU. An instruction that names no operation gets a plain add, which is what a load\/store address, a jump target and @auipc@ all are.
alu :: InstCtrl -> BitVector XLen -> BitVector XLen -> BitVector XLen
alu InstCtrl {aluOp, isOp32} op1 op2 = maybe (op1 + op2) run aluOp
  where
    run op
      | isOp32 = signExtend $ exec op shamt32 (truncateB op1 :: BitVector 32) (truncateB op2)
      | otherwise = exec op shamt64 op1 op2

    shamt64 = unpack $ zeroExtend (truncateB op2 :: BitVector 6)
    shamt32 = unpack $ zeroExtend (truncateB op2 :: BitVector 5)

    exec :: forall n' n. (KnownNat n', n ~ n' + 1) => AluOp -> Int -> BitVector n -> BitVector n -> BitVector n
    exec op shamt a b = case op of
      ADD -> a + b
      SUB -> a - b
      SLL -> a `shiftL` shamt
      SLT -> boolToBV $ signed a < signed b
      SLTU -> boolToBV $ a < b
      XOR -> a `xor` b
      SRL -> a `shiftR` shamt
      SRA -> pack $ signed a `shiftR` shamt
      OR -> a .|. b
      AND -> a .&. b
      where
        signed x = bitCoerce x :: Signed n

-- | Whether the branch is taken. Every condition the type can hold names a branch, so the match is total.
branchUnit :: BranchOp -> BitVector XLen -> BitVector XLen -> Bool
branchUnit cond op1 op2 = case cond of
  BEQ -> beq
  BNE -> not beq
  BLT -> blt
  BGE -> not blt
  BLTU -> bltu
  BGEU -> not bltu
  where
    beq = op1 == op2
    blt = (bitCoerce op1 :: Signed XLen) < (bitCoerce op2 :: Signed XLen)
    bltu = (bitCoerce op1 :: Unsigned XLen) < (bitCoerce op2 :: Unsigned XLen)
