-- | EX: one port per kind of instruction. The ALU ports compute and branch, and the first also accesses the CSR file; the others hand their instruction to a unit or the store queue.
module Cuintet.Stage.Execute (execute, ExecuteIn (..), ExecuteOut (..)) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), InstFormat (..), isCsrRead)
import Cuintet.Eei (Addr, AluOp (..), BranchOp (..), BusWriteReq (..), IssueWidth, LoadQueueAddr, LoadShape (..), MemAccess (..), MemOp (..), NAluPorts, RobAddr, StoreQueueAddr, SystemOp (..), TrapCause, XLen, laneMask, laneOffset, misalignedCause, storeLanes, pattern INSTRUCTION_ADDRESS_MISALIGNED)
import Cuintet.Pipeline (Executed (..), Ready (..))
import Cuintet.Unit.Btb (BtbWrite, predicted, train)
import Cuintet.Unit.Csr (CsrFile, csrAccess)
import Cuintet.Unit.Load (LoadJob (..))
import Cuintet.Unit.LoadQueue (LoadQueueEntry (..))
import Cuintet.Unit.MulDiv (MulDivJob (..))
import Cuintet.Unit.StoreQueue (StoreQueueEntry (..))
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

data ExecuteIn = ExecuteIn
  { entries :: Vec IssueWidth (Maybe Ready)
  , robHead :: RobAddr
  , pendingRedirect :: Maybe RobAddr
  }

data ExecuteOut = ExecuteOut
  { aluCompleted :: Vec NAluPorts (Maybe Executed)
  , storeCompleted :: Maybe Executed
  , mulDivJob :: Maybe MulDivJob
  , loadJob :: Maybe LoadJob
  , storeWrite :: Maybe (StoreQueueAddr, StoreQueueEntry)
  , loadRecord :: Maybe (LoadQueueAddr, LoadQueueEntry)
  , storeSearch :: Maybe (LoadQueueAddr, StoreQueueEntry)
  , wbData :: Vec NAluPorts (BitVector XLen)
  , redirect :: Maybe (RobAddr, Addr)
  , btbWrites :: Vec IssueWidth (Maybe BtbWrite)
  }

execute :: CsrFile -> ExecuteIn -> (CsrFile, ExecuteOut)
execute csrFile ExecuteIn {..} = (csrFile', ExecuteOut {..})
  where
    (aluEntries, rest) = splitAtI entries
    (mulDivEntry, loadEntry, storeEntry) = vecToTuple rest

    -- A CSR instruction issues alone, and only to the first port.
    (csrFile', csrValue) = case head aluEntries of
      Just Ready {ctrl = InstCtrl {systemOp = Just (SysCsr spec)}, rs1Data} -> csrAccess csrFile spec rs1Data
      _ -> (csrFile, deepErrorX "Execute.csrValue")

    (aluCompleted, aluRedirects, aluBtbWrites) = unzip3 (maybe (Nothing, Nothing, Nothing) ((\(e, r, w) -> (Just e, r, w)) . executeAlu csrValue) <$> aluEntries)
    wbData = maybe (deepErrorX "Execute.wbData") (.wbData) <$> aluCompleted

    mulDivException = mulDivEntry >>= (.exception)
    loadException = loadEntry >>= memException
    storeException = storeEntry >>= memException
    (mulDivRedirect, mulDivBtbWrite) = resolveSequential mulDivException mulDivEntry
    (loadRedirect, loadBtbWrite) = resolveSequential loadException loadEntry
    (storeRedirect, storeBtbWrite) = resolveSequential storeException storeEntry

    -- A unit takes its instruction even when it traps, and completes it on the port it shares.
    mulDivJob = do
      Ready {ctrl, rs1Data, rs2Data, pdAddr, robAddr} <- mulDivEntry
      mulDivOp <- ctrl.mulDivOp
      pure MulDivJob {mulDivOp, isOp32 = ctrl.isOp32, op1 = rs1Data, op2 = rs2Data, pdAddr, robAddr, mispredicted = isJust mulDivRedirect, exception = mulDivException}

    loadJob = do
      r@Ready {ctrl, sqAddr, lqAddr, pdAddr, robAddr} <- loadEntry
      Load width sign <- ctrl.memOp
      let addr = memAddr r
      pure LoadJob {addr, shape = LoadShape {width, sign, offset = laneOffset addr}, sqAddr, lqAddr, pdAddr, robAddr, mispredicted = isJust loadRedirect, exception = loadException}

    storeCompleted = do
      r@Ready {ctrl, rs2Data, robAddr} <- storeEntry
      Store width <- ctrl.memOp
      let addr = memAddr r
      pure
        Executed
          { ctrl
          , exception = storeException
          , pdAddr = Nothing
          , robAddr
          , mispredicted = isJust storeRedirect
          , wbData = pack addr
          , mem = orNothing (isNothing storeException) (StoreAccess BusWriteReq {addr, wdata = storeLanes width (laneOffset addr) rs2Data})
          }

    storeWrite = do
      Ready {sqAddr} <- storeEntry
      Executed {mem} <- storeCompleted
      StoreAccess BusWriteReq {addr, wdata} <- mem
      pure (sqAddr, StoreQueueEntry {addr, lanes = wdata})

    loadRecord = do
      LoadJob {addr, shape = LoadShape {width, offset}, lqAddr, exception} <- loadJob
      guard (isNothing exception)
      pure (lqAddr, LoadQueueEntry {addr, mask = laneMask width offset})

    storeSearch = do
      Ready {lqAddr} <- storeEntry
      (_, entry) <- storeWrite
      pure (lqAddr, entry)

    redirects = aluRedirects ++ mulDivRedirect :> loadRedirect :> storeRedirect :> Nil
    btbWrites = aluBtbWrites ++ mulDivBtbWrite :> loadBtbWrite :> storeBtbWrite :> Nil

    -- A younger redirect than a pending one comes from the wrong path.
    redirect = do
      (age, r) <- fold pickOlder (zipWith mk entries redirects)
      guard (maybe True (\p -> age < p - robHead) pendingRedirect)
      pure r
      where
        mk entry target = do
          Ready {robAddr} <- entry
          nextPc <- target
          pure (robAddr - robHead, (robAddr, nextPc))
        pickOlder l r = case (l, r) of
          (Just (x, _), Just (y, _)) -> if y < x then r else l
          (Nothing, _) -> r
          _ -> l
{-# OPAQUE execute #-}

-- | A port without the ALU never branches, but the BTB can still predict its instruction taken by an aliased tag.
resolveSequential :: Maybe (TrapCause, BitVector XLen) -> Maybe Ready -> (Maybe Addr, Maybe BtbWrite)
resolveSequential exception entry = (redirect, btbWrite)
  where
    redirect = do
      Ready {pc, prediction} <- entry
      guard (isNothing exception)
      orNothing (pc + 4 /= predicted pc prediction) (pc + 4)
    btbWrite = do
      Ready {pc, prediction} <- entry
      guard (isNothing exception)
      train pc prediction Nothing

-- | The address of a load or store: the adder alone.
memAddr :: Ready -> Addr
memAddr Ready {rs1Data, imm} = unpack (rs1Data + imm)

memException :: Ready -> Maybe (TrapCause, BitVector XLen)
memException r@Ready {ctrl, exception} =
  exception <|> do
    memOp <- ctrl.memOp
    cause <- misalignedCause memOp (memAddr r)
    pure (cause, pack (memAddr r))

-- | An ALU port: the entry WB takes, the redirect and the BTB training.
executeAlu :: BitVector XLen -> Ready -> (Executed, Maybe Addr, Maybe BtbWrite)
executeAlu csrValue Ready {..} = (executed, redirect, btbWrite)
  where
    executed = Executed {exception = exception', pdAddr = guard (isNothing exception') *> pdAddr, mispredicted = isJust redirect, mem = Nothing, ..}

    (op1, op2) = operands ctrl imm rs1Data rs2Data pc
    aluResult = alu ctrl op1 op2
    branchTaken = maybe False (\cond -> branchUnit cond op1 op2) ctrl.branchOp

    wbData
      | isCsrRead ctrl = csrValue
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

    exception' = exception <|> targetException

    redirect = orNothing (not (squashesAtCommit executed) && nextPc /= predicted pc prediction) nextPc

    btbWrite = guard (isNothing exception') >> train pc prediction (orNothing (nextPc /= pc + 4) nextPc)

squashesAtCommit :: Executed -> Bool
squashesAtCommit executed = isJust executed.exception || executed.ctrl.systemOp == Just SysMret

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
