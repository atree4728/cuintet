-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Completion (regWrite, robWrite)
import Cuintet.CoreCtrl (ExecUnit (..), NExecUnits, Wakeup (..), execUnit, opClassOf, wakeup)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), CommitWidth, DispatchWidth, FetchWidth, IssueWidth, MemReq, MemResp, PRegAddr, RobAddr, WriteBackWidth, XLen)
import Cuintet.Forwarding (Forwarding)
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
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

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
  , rnIssue :: Vec DispatchWidth (Maybe RobAddr)
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

-- | Closes 'coreT' around the register file and the four stage FIFOs.
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
    fetchIn = FetchIn {iResp, buf = fetchedResp, redirect, btbResp}
    decodeIn = DecodeIn {entries = fetchedResp.rdata, wready = decodedResp.wready, stall = flush}
    renameIn = RenameIn {entries = decodedResp.rdata, committed = cmOut.renamed, nextRobAddr = robResp.tl, robFree = robResp.free, flush, drained = robResp.hd == robResp.tl}
    regreadIn = RegReadIn {entries = issueResp.issue, rsData = regResp.rsData, forwards, wready = readyResp.wready}
    executeIn = ExecuteIn {entries = readyEntries, robHead = robResp.hd, wready = executedResp.wready}
    writebackIn = WriteBackIn {entries = executedEntries, loadStoreDone = loadStoreResp.done, mulDivDone = mulDivResp.done, csrWrite = cmOut.csrWrite}
    commitIn = CommitIn {entries = robResp.entries}

    (fetchState', ifOut) = fetch fetchState fetchIn
    idOut = decode decodeIn
    (renameState', rnOut) = rename renameState renameIn
    rrOut = regRead regreadIn
    exOut = execute executeIn
    wbOut = writeback writebackIn
    (csrFile', cmOut) = commit csrFile commitIn

    (mulDivState', mulDivResp) =
      mulDivStep mulDivState MulDivReq {job = exOut.mulDivJob, granted = wbOut.mulDivGranted, squash = cmOut.squash}
    (loadStoreState', loadStoreResp) =
      loadStoreStep loadStoreState LoadStoreReq {job = exOut.loadStoreJob, memResp = dResp, granted = wbOut.loadStoreGranted, squash = cmOut.squash}

    readyEntries = readyResp.rdata
    executedEntries = executedResp.rdata

    forwards = fromEx 1 :> fromEx 0 :> fromWb 1 :> fromWb 0 :> mulDivResp.forwarding :> loadStoreResp.forwarding :> Nil
      where
        fromEx, fromWb :: Index IssueWidth -> Forwarding
        fromEx i = F.forwarding ((.pdAddr) =<< readyEntries !! i) $ do
          entry <- readyEntries !! i
          guard exOut.issued
          orNothing (wakeup (opClassOf entry.ctrl) == AtIssue) (exOut.wbData !! i)
        fromWb i = F.forwarding ((.pdAddr) =<< executedEntries !! i) $ do
          entry <- executedEntries !! i
          orNothing (wakeup (opClassOf entry.ctrl) == AtIssue) entry.wbData

    -- A younger redirect than a pending one comes from the wrong path; the squash at retire ends the pending one.
    exRedirect = do
      (robAddr, pc) <- exOut.redirect
      guard (maybe True (\p -> robAddr - robResp.hd < p - robResp.hd) pendingRedirect)
      pure (robAddr, pc)
    pendingRedirect'
      | cmOut.squash = Nothing
      | otherwise = (fst <$> exRedirect) <|> pendingRedirect

    redirect = cmOut.redirect <|> (snd <$> exRedirect)
    flush = isJust redirect
    rsAddrs = concatMap (maybe (repeat 0) (\Renamed {..} -> ps1Addr :> ps2Addr :> Nil)) issueResp.issue
    regReq = RegReq {rsAddrs, writes = (regWrite =<<) <$> wbOut.completions}
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}

    inflightTo unit = any (maybe False ((== Just unit) . execUnit . opClassOf . (.ctrl)))
    busy =
      (mulDivResp.busy || inflightTo MulDivUnit readyEntries)
        :> (loadStoreResp.busy || inflightTo MemUnit readyEntries)
        :> Nil

    fetchedReq = RingReq {wdata = ifOut.issue, pop = nIssued idOut.issue, squash = flush}
    decodedReq = FifoReq {wdata = idOut.issue, rready = any isJust rnOut.issue, flush}
    wakeups =
      atIssue 0
        :> atIssue 1
        :> broadcasted mulDivResp.forwarding
        :> broadcasted loadStoreResp.forwarding
        :> (fst <$> cmOut.csrWrite)
        :> Nil
      where
        atIssue :: Index IssueWidth -> Maybe PRegAddr
        atIssue i = do
          entry <- rrOut.issue !! i
          guard (wakeup (opClassOf entry.ctrl) == AtIssue)
          entry.pdAddr
        broadcasted = \case
          F.Ready pd _ -> Just pd
          F.Idle -> Nothing

    issueReq = IssueQueueReq {dispatch = rnOut.issue, accepted = isJust <$> rrOut.issue, robHead = robResp.hd, busy, wakeup = wakeups, squash = cmOut.squash}
    readyReq = FifoReq {wdata = rrOut.issue, rready = exOut.issued, flush = cmOut.squash}
    executedReq = FifoReq {wdata = exOut.completed, rready = wbOut.issued, flush = cmOut.squash}
    robReq = RobReq {allocates = rnOut.allocates, completes = (robWrite =<<) <$> wbOut.completions, pop = cmOut.pop, squash = cmOut.squash}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = loadStoreResp.memReq, retired = cmOut.retired, led = csrFile.led, coreTrace}
    coreTrace =
      CoreTrace
        { ifStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , ifIssue = if flush then repeat Nothing else ifOut.issue
        , idIssue = nIssued idOut.issue
        , rnIssue = fmap fst <$> rnOut.allocates
        , rrIssue = fmap (.robAddr) <$> rrOut.issue
        , exHold = fmap (.robAddr) <$> readyEntries
        , unitHold = mulDivHolder :> loadStoreHolder :> Nil
        , wbHold = fmap (.robAddr) <$> executedEntries
        , wbComplete = fmap fst . (robWrite =<<) <$> wbOut.completions
        , robHead = robResp.hd
        , robTail = robResp.tl
        , cmRetire = cmOut.retired
        , flush
        }
      where
        mulDivHolder = case mulDivState of
          M.Busy job _ -> Just job.robAddr
          M.Waiting c -> fst <$> robWrite c
          M.Idle -> Nothing
        loadStoreHolder = case loadStoreState of
          L.WaitReady job _ -> Just job.robAddr
          L.WaitValid job _ -> Just job.robAddr
          L.Waiting c -> fst <$> robWrite c
          L.Idle -> Nothing

    state' = CoreState {fetchState = fetchState', renameState = renameState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile', pendingRedirect = pendingRedirect'}

nIssued :: Vec IssueWidth (Maybe a) -> Index (DispatchWidth + 1)
nIssued = sum . fmap (\entry -> if isJust entry then 1 else 0)
