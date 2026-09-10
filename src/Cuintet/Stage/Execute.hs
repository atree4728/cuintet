-- | EX: operand selection, the ALU, the branch condition, and the multiply\/divide unit.
module Cuintet.Stage.Execute (execute, ExecuteIn (..), ExecuteOut (..)) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.CoreCtrl (ExecUnit (..), InstCtrl (..), InstFormat (..), execUnit, isCsrRead, opClassOf)
import Cuintet.Eei (Addr, AluOp (..), BranchOp (..), DispatchWidth, XLen, misalignedCause, pattern INSTRUCTION_ADDRESS_MISALIGNED)
import Cuintet.Pipeline (Executed (..), Ready (..), isSerializing)
import Cuintet.Unit.Btb (BtbWrite, predicted, train)
import Cuintet.Unit.MulDiv (MulDivJob (..))
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

data ExecuteIn = ExecuteIn
  { entries :: Upto DispatchWidth Ready
  , wready :: Bool
  , mulDivBusy :: Bool
  }

data ExecuteOut = ExecuteOut
  { issue :: Upto DispatchWidth Executed
  -- ^ The group handed to MA, lane 1 dropped when it turned out to be down the wrong path.
  , issued :: Bool
  -- ^ Whether the group leaves EX this clock, which 'redirect' must not feed into.
  , wbData :: Vec DispatchWidth (BitVector XLen)
  -- ^ For the broadcast, so before the squash.
  , redirect :: Maybe Addr
  , btbWrites :: Vec DispatchWidth (Maybe BtbWrite)
  , mulDivJob :: Maybe MulDivJob
  }

execute :: ExecuteIn -> ExecuteOut
execute ExecuteIn {..} = ExecuteOut {..}
  where
    needsMulDiv = maybe False (\Ready {..} -> isNothing exception && execUnit (opClassOf ctrl) == Just MulDivUnit) (Upto.head entries)
    issued = entries.len > 0 && wready && not (needsMulDiv && mulDivBusy)

    mulDivJob = guard issued *> (mkJob executed0.mispredicted =<< Upto.head entries)
    mkJob mispredicted Ready {..}
      | isNothing exception
      , Just mulDivOp <- ctrl.mulDivOp =
          Just MulDivJob {mulDivOp, isOp32 = ctrl.isOp32, op1 = rs1Data, op2 = rs2Data, pdAddr, robAddr, mispredicted}
      | otherwise = Nothing

    ((executed0, redirect0, btbWrite0), (executed1, redirect1, btbWrite1)) = vecToTuple $ executeLane <$> entries.elems

    squash1 = isJust redirect0 || isSerializing executed0

    len
      | not issued = 0
      | entries.len == 2 && not squash1 = 2
      | otherwise = 1

    issue = Upto {len, elems = executed0 :> executed1 :> Nil}
    wbData = (.wbData) <$> issue.elems

    redirect
      | len == 2 = redirect0 <|> redirect1
      | len == 1 = redirect0
      | otherwise = Nothing

    btbWrites = (guard issued >> btbWrite0) :> (guard (issued && entries.len == 2) >> btbWrite1) :> Nil
{-# OPAQUE execute #-}

executeLane :: Ready -> (Executed, Maybe Addr, Maybe BtbWrite)
executeLane Ready {..} = (executed, redirect, btbWrite)
  where
    executed = Executed {exception = exception', mispredicted = isJust redirect, ..}

    (op1, op2) = operands ctrl imm rs1Data rs2Data pc
    aluResult = alu ctrl op1 op2
    branchTaken = maybe False (\cond -> branchUnit cond op1 op2) ctrl.branchOp

    wbData
      | isCsrRead ctrl = rs1Data
      | ctrl.isLui = imm
      | ctrl.isJump = bitCoerce (pc + 4)
      | otherwise = aluResult

    nextPc
      | ctrl.isJump = unpack (aluResult .&. complement 1)
      | branchTaken = pc + numConvert imm
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

    redirect = orNothing (not (isSerializing executed) && nextPc /= predicted pc prediction) nextPc

    btbWrite = guard (isNothing exception') >> train pc prediction (orNothing (nextPc /= pc + 4) nextPc)

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
