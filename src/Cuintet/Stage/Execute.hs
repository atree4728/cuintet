-- | EX: the entry of every unit. Operand selection, the ALU and the branch condition happen here, and the multiply\/divide and load\/store jobs leave from here.
module Cuintet.Stage.Execute (execute, ExecuteIn (..), ExecuteOut (..)) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), InstFormat (..), execUnit, isCsrRead, opClassOf)
import Cuintet.Eei (Addr, AluOp (..), BranchOp (..), IssueWidth, RobAddr, XLen, misalignedCause, pattern INSTRUCTION_ADDRESS_MISALIGNED)
import Cuintet.Pipeline (Executed (..), Ready (..), isSerializing)
import Cuintet.Unit.Btb (BtbWrite, predicted, train)
import Cuintet.Unit.LoadStore (LoadStoreJob (..))
import Cuintet.Unit.MulDiv (MulDivJob (..))
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

data ExecuteIn = ExecuteIn
  { entries :: Vec IssueWidth (Maybe Ready)
  , robHead :: RobAddr
  , wready :: Bool
  }

data ExecuteOut = ExecuteOut
  { completed :: Vec IssueWidth (Maybe Executed)
  , mulDivJob :: Maybe MulDivJob
  , loadStoreJob :: Maybe LoadStoreJob
  , issued :: Bool
  , wbData :: Vec IssueWidth (BitVector XLen)
  , redirect :: Maybe Addr
  , btbWrites :: Vec IssueWidth (Maybe BtbWrite)
  }

execute :: ExecuteIn -> ExecuteOut
execute ExecuteIn {..} = ExecuteOut {..}
  where
    issued = any isJust entries && wready

    lanes = fmap executeLane <$> entries

    completed = zipWith keep entries lanes
      where
        keep entry lane = do
          guard issued
          Ready {ctrl} <- entry
          Lane {executed} <- lane
          guard (isNothing (execUnit (opClassOf ctrl)) || isJust executed.exception)
          pure executed

    wbData = maybe (deepErrorX "Execute.wbData") (\lane -> lane.executed.wbData) <$> lanes

    port0 = do
      guard issued
      ready <- head entries
      lane <- head lanes
      guard (isNothing lane.executed.exception)
      pure (ready, lane)

    mulDivJob = do
      (Ready {ctrl, rs1Data, rs2Data, pdAddr, robAddr}, Lane {executed}) <- port0
      mulDivOp <- ctrl.mulDivOp
      pure MulDivJob {mulDivOp, isOp32 = ctrl.isOp32, op1 = rs1Data, op2 = rs2Data, pdAddr, robAddr, mispredicted = executed.mispredicted}

    loadStoreJob = do
      (Ready {ctrl, rs2Data, pdAddr, robAddr}, Lane {executed, aluResult}) <- port0
      memOp <- ctrl.memOp
      pure LoadStoreJob {memOp, addr = bitCoerce aluResult, wdata = rs2Data, pdAddr, robAddr, mispredicted = executed.mispredicted}

    redirect = do
      guard issued
      snd <$> fold pickOlder (zipWith mk entries lanes)
      where
        mk entry lane = do
          Ready {robAddr} <- entry
          nextPc <- lane >>= (.redirect)
          pure (robAddr - robHead, nextPc)
        pickOlder l r = case (l, r) of
          (Just (x, _), Just (y, _)) -> if y < x then r else l
          (Nothing, _) -> r
          _ -> l

    btbWrites = (>>= \lane -> guard issued >> lane.btbWrite) <$> lanes
{-# OPAQUE execute #-}

-- | What one port produced: the entry WB may take, plus what only EX itself needs.
data Lane = Lane
  { executed :: Executed
  , aluResult :: BitVector XLen
  -- ^ The load\/store address, which no later stage recomputes.
  , redirect :: Maybe Addr
  , btbWrite :: Maybe BtbWrite
  }

executeLane :: Ready -> Lane
executeLane Ready {..} = Lane {executed, aluResult, redirect, btbWrite}
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
