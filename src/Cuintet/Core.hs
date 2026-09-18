-- | The out-of-order core in terms of the IF, ID, RN, RR, EX, WB and Cm stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Completion (robWrite)
import Cuintet.CoreCtrl (ExecUnit (..), InstCtrl, NExecUnits, Wakeup (..), execUnit, isLoad, isStore, opClassOf, wakeup)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), CommitWidth, DispatchWidth, FetchWidth, IssueWidth, MemReq, MemResp, NAluPorts, PRegAddr, RobAddr, WriteBackWidth, XLen)
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
import Cuintet.Unit.Load (LoadReq (..), LoadState, loadStep)
import Cuintet.Unit.Load qualified as L
import Cuintet.Unit.LoadQueue (LoadQueueReq (..), LoadQueueResp (..), loadQueue)
import Cuintet.Unit.MulDiv (MulDivReq (..), MulDivState, mulDivStep)
import Cuintet.Unit.MulDiv qualified as M
import Cuintet.Unit.RegFile (RegReq (..), RegResp (..), regFile)
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
import Cuintet.Unit.Rob (RobReq (..), RobResp (..), rob)
import Cuintet.Unit.StoreQueue (StoreQueueReq (..), StoreQueueResp (..), storeQueue)
import Cuintet.Util ((<<$>>))
import Data.Bool (bool)
import Data.Maybe (isJust)
import GHC.Records (HasField)

data CoreIn = CoreIn
  { iResp :: MemResp
  , dReadResp :: MemResp
  , dWriteResp :: MemResp
  }

data CoreOut = CoreOut
  { iReq :: Maybe MemReq
  , dReadReq :: Maybe MemReq
  , dWriteReq :: Maybe MemReq
  , retired :: Vec CommitWidth (Maybe Retire)
  , led :: BitVector XLen
  , coreTrace :: CoreTrace
  }

-- | The core's state.
data CoreState = CoreState
  { fetchState :: FetchState
  , renameState :: RenameState
  , ready :: Vec IssueWidth (Maybe Ready)
  , executed :: Vec NAluPorts (Maybe Executed)
  , storeExecuted :: Maybe Executed
  , mulDivState :: MulDivState
  , loadState :: LoadState
  , csrFile :: CsrFile
  , pendingRedirect :: Maybe RobAddr
  -- ^ The oldest entry whose redirect IF has taken and Cm has not yet squashed.
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState = CoreState {fetchState = initFetchState, renameState = initRenameState, ready = repeat Nothing, executed = repeat Nothing, storeExecuted = Nothing, mulDivState = M.Idle, loadState = L.Idle, csrFile = initCsrFile, pendingRedirect = Nothing}

-- | From the ROB on, the stages are told by which entry each of them holds in the clock.
data CoreTrace = CoreTrace
  { ifStart :: Maybe Addr
  , ifIssue :: Vec FetchWidth (Maybe Fetched)
  , idIssue :: Index (DispatchWidth + 1)
  , rnIssue :: Vec DispatchWidth (Maybe Renamed)
  , rrIssue :: Vec IssueWidth (Maybe RobAddr)
  , exHold :: Vec IssueWidth (Maybe RobAddr)
  , unitHold :: Vec NExecUnits (Maybe RobAddr)
  , wbHold :: Vec 3 (Maybe RobAddr)
  , wbComplete :: Vec (WriteBackWidth + 1) (Maybe RobAddr)
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
    (coreOut, regReq, btbReq, robReq, sqReq, lqReq, fetchedReq, decodedReq, issueReq) =
      mealyB coreT initState (coreIn, regResp, btbResp, robResp, sqResp, lqResp, fetchedResp, decodedResp, issueResp)
    btbResp = btb btbReq
    regResp = regFile regReq
    robResp = rob robReq
    sqResp = storeQueue sqReq
    lqResp = loadQueue lqReq
    fetchedResp = ring (SNat @FetchBufBits) fetchedReq
    decodedResp = fifo decodedReq
    issueResp = issueQueue issueReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  ( CoreIn
  , RegResp
  , BtbResp
  , RobResp
  , StoreQueueResp
  , LoadQueueResp
  , RingResp FetchBufBits DispatchWidth Fetched
  , FifoResp DispatchWidth Decoded
  , IssueQueueResp
  ) ->
  ( CoreState
  , ( CoreOut
    , RegReq
    , BtbReq
    , RobReq
    , StoreQueueReq
    , LoadQueueReq
    , RingReq FetchBufBits FetchWidth DispatchWidth Fetched
    , FifoReq DispatchWidth Decoded
    , IssueQueueReq
    )
  )
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, robResp, sqResp, lqResp, fetchedResp, decodedResp, issueResp) =
  (state', (coreOut, regReq, btbReq, robReq, sqReq, lqReq, fetchedReq, decodedReq, issueReq))
  where
    (fetchState', ifOut) = fetch fetchState FetchIn {iResp, buf = fetchedResp, redirect, btbResp}
    idOut = decode DecodeIn {entries = fetchedResp.rdata, wready = decodedResp.wready, stall = flush}
    (renameState', rnOut) = rename renameState RenameIn {entries = decodedResp.rdata, committed = cmOut.renamed, nextRobAddr = robResp.tl, robFree = robResp.free, nextSqAddr = sqResp.tl, sqFree = sqResp.free, nextLqAddr = lqResp.tl, flush, drained = robResp.hd == robResp.tl}
    rrOut = regRead RegReadIn {entries = issueResp.issue, rsData = regResp.rsData, bypasses}
    exOut = execute ExecuteIn {entries = ready, robHead = robResp.hd, pendingRedirect}
    wbOut = writeback WriteBackIn {alus = executed, store = storeExecuted, loadDone = loadResp.done, mulDivDone = mulDivResp.done, csrWrite = cmOut.csrWrite}
    (csrFile', cmOut) = commit csrFile CommitIn {entries = robResp.entries, orderFail = lqResp.orderFail}

    (mulDivState', mulDivResp) =
      mulDivStep mulDivState MulDivReq {job = exOut.mulDivJob, granted = wbOut.mulDivGranted, squash = cmOut.squash}
    (loadState', loadResp) =
      loadStep loadState LoadReq {job = exOut.loadJob, sq = sqResp, dReadResp, granted = wbOut.loadGranted, squash = cmOut.squash}

    redirect = cmOut.redirect <|> (snd <$> exOut.redirect)
    flush = isJust redirect
    pendingRedirect' = guard (not cmOut.squash) *> ((fst <$> exOut.redirect) <|> pendingRedirect)

    -- AtIssue: RR wakes, then EX and WB bypass until the register file has it; only the ALU ports have such instructions.
    -- AtComplete: a unit wakes and bypasses while it holds the completion.
    -- AtCommit: Cm wakes as WB writes.
    wakeups = (atIssue <$> takeI rrOut.issue) ++ (fst <<$>> unitBypasses) ++ (fst <$> cmOut.csrWrite) :> Nil
    bypasses =
      reverse unitBypasses
        ++ zipWith (liftA2 (,)) (atIssue <$> executed) ((.wbData) <<$>> executed)
        ++ zipWith (liftA2 (,)) (atIssue <$> takeI ready) (Just <$> exOut.wbData)
    unitBypasses = mulDivResp.bypass :> loadResp.bypass :> Nil

    busy = (mulDivResp.busy || inflightTo MulDivUnit) :> (loadResp.busy || inflightTo LoadUnit) :> Nil
      where
        inflightTo unit = any (maybe False ((== Just unit) . execUnit . opClassOf . (.ctrl))) ready
    rsAddrs = concatMap (maybe (repeat 0) (\Renamed {..} -> ps1Addr :> ps2Addr :> Nil)) issueResp.issue
    idIssue = sum $ bool 0 1 . isJust <$> idOut.issue

    regReq = RegReq {rsAddrs, writes = wbOut.regWrites}
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}
    renamedCount p = sum (bool 0 1 . maybe False (p . (.ctrl)) <$> rnOut.issue)
    sqReq = StoreQueueReq {allocates = renamedCount isStore, write = exOut.storeWrite, commits = cmOut.stores, dWriteResp, squash = cmOut.squash}
    lqReq = LoadQueueReq {allocates = renamedCount isLoad, record = exOut.loadRecord, store = exOut.storeSearch, pops = cmOut.loads, squash = cmOut.squash}
    robReq = RobReq {allocates = rnOut.allocates, completes = wbOut.robWrites, pop = cmOut.pop, squash = cmOut.squash}
    fetchedReq = RingReq {wdata = ifOut.issue, pop = idIssue, squash = flush}
    decodedReq = FifoReq {wdata = idOut.issue, rready = any isJust rnOut.issue, flush}
    issueReq = IssueQueueReq {dispatch = rnOut.issue, accepted = isJust <$> rrOut.issue, robHead = robResp.hd, busy, wakeup = wakeups, squash = cmOut.squash}

    coreOut = CoreOut {iReq = ifOut.iReq, dReadReq = loadResp.dReadReq, dWriteReq = sqResp.storeReq, retired = cmOut.retired, led = csrFile.led, coreTrace}
    coreTrace =
      CoreTrace
        { ifStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , ifIssue = if flush then repeat Nothing else ifOut.issue
        , idIssue
        , rnIssue = rnOut.issue
        , rrIssue = (.robAddr) <<$>> rrOut.issue
        , exHold = (.robAddr) <<$>> ready
        , unitHold = mulDivHolder mulDivState :> loadHolder loadState :> Nil
        , wbHold = ((.robAddr) <<$>> executed) :< ((.robAddr) <$> storeExecuted)
        , wbComplete = fmap fst <$> wbOut.robWrites
        , robHead = robResp.hd
        , robTail = robResp.tl
        , cmRetire = cmOut.retired
        , flush
        }

    state' =
      CoreState
        { fetchState = fetchState'
        , renameState = renameState'
        , ready = if cmOut.squash then repeat Nothing else rrOut.issue
        , executed = if cmOut.squash then repeat Nothing else exOut.aluCompleted
        , storeExecuted = guard (not cmOut.squash) *> exOut.storeCompleted
        , mulDivState = mulDivState'
        , loadState = loadState'
        , csrFile = csrFile'
        , pendingRedirect = pendingRedirect'
        }

atIssue :: (HasField "ctrl" stage InstCtrl, HasField "pdAddr" stage (Maybe PRegAddr)) => Maybe stage -> Maybe PRegAddr
atIssue entry = do
  e <- entry
  guard (wakeup (opClassOf e.ctrl) == AtIssue)
  e.pdAddr

mulDivHolder :: MulDivState -> Maybe RobAddr
mulDivHolder = \case
  M.Busy job _ -> Just job.robAddr
  M.Waiting c -> fst <$> robWrite c
  M.Idle -> Nothing

loadHolder :: LoadState -> Maybe RobAddr
loadHolder = \case
  L.WaitReady job -> Just job.robAddr
  L.WaitValid job _ -> Just job.robAddr
  L.Waiting c -> fst <$> robWrite c
  L.Idle -> Nothing
