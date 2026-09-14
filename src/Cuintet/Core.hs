-- | The out-of-order core in terms of the IF, ID, RN, RR, EX, WB and Cm stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Completion (regWrite, robWrite)
import Cuintet.CoreCtrl (ExecUnit (..), InstCtrl, NExecUnits, Wakeup (..), execUnit, opClassOf, wakeup)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), CommitWidth, DispatchWidth, FetchWidth, IssueWidth, MemReq, MemResp, PRegAddr, RobAddr, WriteBackWidth, XLen)
import Cuintet.Forwarding qualified as F
import Cuintet.Pipeline (Decoded (..), Executed (..), FetchBufBits, Fetched (..), Ready (..), Renamed (..), Retire (..))
import Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead)
import Cuintet.Stage.Rename (RenameIn (..), RenameOut (..), RenameState, initRenameState, rename)
import Cuintet.Stage.WriteBack (WriteBackIn (..), WriteBackOut (..), writeback)
import Cuintet.Unit.Btb (BtbReq (..), BtbResp, btb)
import Cuintet.Unit.Csr (CsrFile (led), initCsrFile)
import Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.Unit.IssueQueue (IssueQueueReq (..), IssueQueueResp (..), issueQueue)
import Cuintet.Unit.LoadStore (LoadStoreReq (..), LoadStoreState, loadStoreStep)
import Cuintet.Unit.LoadStore qualified as L
import Cuintet.Unit.MulDiv (MulDivReq (..), MulDivState, mulDivStep)
import Cuintet.Unit.MulDiv qualified as M
import Cuintet.Unit.RegFile (RegReq (..), RegResp (..), regFile)
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
import Cuintet.Unit.Rob (RobReq (..), RobResp (..), rob)
import Data.Maybe (isJust)
import GHC.Records (HasField)

data CoreIn = CoreIn
  { iResp :: MemResp
  -- ^ Response to the instruction fetch request.
  , dResp :: MemResp
  -- ^ Response to the load/store request.
  }

data CoreOut = CoreOut
  { iReq :: Maybe MemReq
  -- ^ Instruction fetch request.
  , dReq :: Maybe MemReq
  -- ^ Load/store request.
  , retired :: Vec CommitWidth (Maybe Retire)
  -- ^ Execution log of a single instruction, emitted only in the clock it retires.
  , led :: BitVector XLen
  , coreTrace :: CoreTrace
  }

-- | The core's state.
data CoreState = CoreState
  { fetchState :: FetchState
  , renameState :: RenameState
  , mulDivState :: MulDivState
  , loadStoreState :: LoadStoreState
  , csrFile :: CsrFile
  , pendingRedirect :: Maybe RobAddr
  -- ^ The oldest entry whose redirect IF has taken and Cm has not yet squashed.
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState = CoreState {fetchState = initFetchState, renameState = initRenameState, mulDivState = M.Idle, loadStoreState = L.Idle, csrFile = initCsrFile, pendingRedirect = Nothing}

-- | From the ROB on, the stages are told by which entry each of them holds in the clock.
data CoreTrace = CoreTrace
  { ifStart :: Maybe Addr
  , ifIssue :: Vec FetchWidth (Maybe Fetched)
  , idIssue :: Index (DispatchWidth + 1)
  , rnIssue :: Vec DispatchWidth (Maybe Renamed)
  , rrIssue :: Vec IssueWidth (Maybe RobAddr)
  , exHold :: Vec IssueWidth (Maybe RobAddr)
  , unitHold :: Vec NExecUnits (Maybe RobAddr)
  , wbHold :: Vec IssueWidth (Maybe RobAddr)
  , wbComplete :: Vec WriteBackWidth (Maybe RobAddr)
  , robHead :: RobAddr
  , robTail :: RobAddr
  , cmRetire :: Vec CommitWidth (Maybe Retire)
  , flush :: Bool
  }
  deriving (Generic, NFDataX)

-- | Closes 'coreT' around the register file, the BTB, the ROB, the issue queue and the stage buffers.
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core coreIn = coreOut
  where
    (coreOut, regReq, btbReq, robReq, fetchedReq, decodedReq, issueReq, readyReq, executedReq) =
      mealyB coreT initState (coreIn, regResp, btbResp, robResp, fetchedResp, decodedResp, issueResp, readyResp, executedResp)
    btbResp = btb btbReq
    regResp = regFile regReq
    robResp = rob robReq
    fetchedResp = ring (SNat @FetchBufBits) fetchedReq
    decodedResp = fifo decodedReq
    issueResp = issueQueue issueReq
    readyResp = fifo readyReq
    executedResp = fifo executedReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  ( CoreIn
  , RegResp
  , BtbResp
  , RobResp
  , RingResp FetchBufBits DispatchWidth Fetched
  , FifoResp DispatchWidth Decoded
  , IssueQueueResp
  , FifoResp IssueWidth Ready
  , FifoResp IssueWidth Executed
  ) ->
  ( CoreState
  , ( CoreOut
    , RegReq
    , BtbReq
    , RobReq
    , RingReq FetchBufBits FetchWidth DispatchWidth Fetched
    , FifoReq DispatchWidth Decoded
    , IssueQueueReq
    , FifoReq IssueWidth Ready
    , FifoReq IssueWidth Executed
    )
  )
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, robResp, fetchedResp, decodedResp, issueResp, readyResp, executedResp) =
  (state', (coreOut, regReq, btbReq, robReq, fetchedReq, decodedReq, issueReq, readyReq, executedReq))
  where
    (fetchState', ifOut) = fetch fetchState FetchIn {iResp, buf = fetchedResp, redirect, btbResp}
    idOut = decode DecodeIn {entries = fetchedResp.rdata, wready = decodedResp.wready, stall = flush}
    (renameState', rnOut) = rename renameState RenameIn {entries = decodedResp.rdata, committed = cmOut.renamed, nextRobAddr = robResp.tl, robFree = robResp.free, flush, drained = robResp.hd == robResp.tl}
    rrOut = regRead RegReadIn {entries = issueResp.issue, rsData = regResp.rsData, forwards, wready = readyResp.wready}
    exOut = execute ExecuteIn {entries = readyResp.rdata, robHead = robResp.hd, pendingRedirect, wready = executedResp.wready}
    wbOut = writeback WriteBackIn {entries = executedResp.rdata, loadStoreDone = loadStoreResp.done, mulDivDone = mulDivResp.done, csrWrite = cmOut.csrWrite}
    (csrFile', cmOut) = commit csrFile CommitIn {entries = robResp.entries}

    (mulDivState', mulDivResp) =
      mulDivStep mulDivState MulDivReq {job = exOut.mulDivJob, granted = wbOut.mulDivGranted, squash = cmOut.squash}
    (loadStoreState', loadStoreResp) =
      loadStoreStep loadStoreState LoadStoreReq {job = exOut.loadStoreJob, memResp = dResp, granted = wbOut.loadStoreGranted, squash = cmOut.squash}

    redirect = cmOut.redirect <|> (snd <$> exOut.redirect)
    flush = isJust redirect
    pendingRedirect' = guard (not cmOut.squash) *> ((fst <$> exOut.redirect) <|> pendingRedirect)

    -- AtIssue: RR wakes, then EX and WB forward until the register file has it.
    -- AtComplete: a unit wakes and forwards while it holds the completion.
    -- AtCommit: Cm wakes as WB writes.
    wakeups = (atIssue <$> rrOut.issue) ++ (F.dest <$> unitForwards) ++ (fst <$> cmOut.csrWrite) :> Nil
    forwards =
      reverse (zipWith F.forwarding (atIssue <$> readyResp.rdata) ((<$ guard exOut.issued) <$> exOut.wbData))
        ++ reverse (zipWith F.forwarding (atIssue <$> executedResp.rdata) (fmap (.wbData) <$> executedResp.rdata))
        ++ unitForwards
    unitForwards = mulDivResp.forwarding :> loadStoreResp.forwarding :> Nil

    busy = (mulDivResp.busy || inflightTo MulDivUnit) :> (loadStoreResp.busy || inflightTo MemUnit) :> Nil
      where
        inflightTo unit = any (maybe False ((== Just unit) . execUnit . opClassOf . (.ctrl))) readyResp.rdata
    rsAddrs = concatMap (maybe (repeat 0) (\Renamed {..} -> ps1Addr :> ps2Addr :> Nil)) issueResp.issue
    idIssue = sum . fmap (\entry -> if isJust entry then 1 else 0) $ idOut.issue

    regReq = RegReq {rsAddrs, writes = (regWrite =<<) <$> wbOut.completions}
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}
    robReq = RobReq {allocates = rnOut.allocates, completes = (robWrite =<<) <$> wbOut.completions, pop = cmOut.pop, squash = cmOut.squash}
    fetchedReq = RingReq {wdata = ifOut.issue, pop = idIssue, squash = flush}
    decodedReq = FifoReq {wdata = idOut.issue, rready = any isJust rnOut.issue, flush}
    issueReq = IssueQueueReq {dispatch = rnOut.issue, accepted = isJust <$> rrOut.issue, robHead = robResp.hd, busy, wakeup = wakeups, squash = cmOut.squash}
    readyReq = FifoReq {wdata = rrOut.issue, rready = exOut.issued, flush = cmOut.squash}
    executedReq = FifoReq {wdata = exOut.completed, rready = wbOut.issued, flush = cmOut.squash}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = loadStoreResp.memReq, retired = cmOut.retired, led = csrFile.led, coreTrace}
    coreTrace =
      CoreTrace
        { ifStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , ifIssue = if flush then repeat Nothing else ifOut.issue
        , idIssue
        , rnIssue = rnOut.issue
        , rrIssue = fmap (.robAddr) <$> rrOut.issue
        , exHold = fmap (.robAddr) <$> readyResp.rdata
        , unitHold = mulDivHolder mulDivState :> loadStoreHolder loadStoreState :> Nil
        , wbHold = fmap (.robAddr) <$> executedResp.rdata
        , wbComplete = fmap fst . (robWrite =<<) <$> wbOut.completions
        , robHead = robResp.hd
        , robTail = robResp.tl
        , cmRetire = cmOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', renameState = renameState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile', pendingRedirect = pendingRedirect'}

atIssue :: (HasField "ctrl" r InstCtrl, HasField "pdAddr" r (Maybe PRegAddr)) => Maybe r -> Maybe PRegAddr
atIssue entry = do
  e <- entry
  guard (wakeup (opClassOf e.ctrl) == AtIssue)
  e.pdAddr

mulDivHolder :: MulDivState -> Maybe RobAddr
mulDivHolder = \case
  M.Busy job _ -> Just job.robAddr
  M.Waiting c -> fst <$> robWrite c
  M.Idle -> Nothing

loadStoreHolder :: LoadStoreState -> Maybe RobAddr
loadStoreHolder = \case
  L.WaitReady job _ -> Just job.robAddr
  L.WaitValid job _ -> Just job.robAddr
  L.Waiting c -> fst <$> robWrite c
  L.Idle -> Nothing
