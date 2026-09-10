-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), CommitWidth, DispatchWidth, FetchWidth, MemReq, MemResp, XLen)
import Cuintet.Forwarding (Forwarding)
import Cuintet.Forwarding qualified as F
import Cuintet.Pipeline (Decoded (..), Executed (..), FetchBufBits, Fetched (..), Ready (..), Renamed (..), Retire (..), hasResult, pdOf, regWrite, robWrite)
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
import Cuintet.Unit.LoadStore (LoadStoreReq (..), LoadStoreState, loadStoreStep)
import Cuintet.Unit.LoadStore qualified as L
import Cuintet.Unit.MulDiv (MulDivReq (..), MulDivState, mulDivStep)
import Cuintet.Unit.MulDiv qualified as M
import Cuintet.Unit.RegFile (RegReq (..), RegResp (..), regFile)
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
import Cuintet.Unit.Rob (RobReq (..), RobResp (..), rob)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
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
  , serializingInFlight :: Bool
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState = CoreState {fetchState = initFetchState, renameState = initRenameState, mulDivState = M.Idle, loadStoreState = L.Idle, csrFile = initCsrFile, serializingInFlight = False}

data CoreTrace = CoreTrace
  { fetchStart :: Maybe Addr
  , ifIssue :: Upto FetchWidth Fetched
  , idIssue :: Index (DispatchWidth + 1)
  , rnIssue :: Index (DispatchWidth + 1)
  , rrIssue :: Index (DispatchWidth + 1)
  , exIssue :: Index (DispatchWidth + 1)
  , wbIssue :: Index (DispatchWidth + 1)
  , retired :: Vec CommitWidth (Maybe Retire)
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
    (coreOut, regReq, btbReq, robReq, fetchedReq, decodedReq, renamedReq, readyReq, executedReq) =
      mealyB coreT initState (coreIn, regResp, btbResp, robResp, fetchedResp, decodedResp, renamedResp, readyResp, executedResp)
    btbResp = btb btbReq
    regResp = regFile regReq
    robResp = rob robReq
    fetchedResp = ring (SNat @FetchBufBits) fetchedReq
    decodedResp = fifo decodedReq
    renamedResp = fifo renamedReq
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
  , FifoResp DispatchWidth Renamed
  , FifoResp DispatchWidth Ready
  , FifoResp DispatchWidth Executed
  ) ->
  ( CoreState
  , ( CoreOut
    , RegReq
    , BtbReq
    , RobReq
    , RingReq FetchBufBits FetchWidth DispatchWidth Fetched
    , FifoReq DispatchWidth Decoded
    , FifoReq DispatchWidth Renamed
    , FifoReq DispatchWidth Ready
    , FifoReq DispatchWidth Executed
    )
  )
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, robResp, fetchedResp, decodedResp, renamedResp, readyResp, executedResp) =
  (state', (coreOut, regReq, btbReq, robReq, fetchedReq, decodedReq, renamedReq, readyReq, executedReq))
  where
    fetchIn = FetchIn {iResp, buf = fetchedResp, redirect, btbResp}
    decodeIn = DecodeIn {entries = fetchedResp.rdata, wready = decodedResp.wready, stall = flush}
    renameIn = RenameIn {entries = decodedResp.rdata, committed = cmOut.renamed, nextRobAddr = robResp.buffer.tl, robFree = robResp.buffer.free, flush, drained = robResp.buffer.rdata.len == 0, wready = renamedResp.wready}
    regreadIn = RegReadIn {entries = renamedResp.rdata, rsData = regResp.rsData, forwards, wready = readyResp.wready}
    executeIn = ExecuteIn {entries = readyResp.rdata, wready = executedResp.wready, mulDivBusy = mulDivResp.busy}
    writebackIn = WriteBackIn {entries = executedResp.rdata, loadStoreBusy = loadStoreResp.busy, loadStoreDone = loadStoreResp.done, mulDivDone = mulDivResp.done, csrWrite = cmOut.csrWrite, serializingInFlight}
    commitIn = CommitIn {entries = robResp.buffer.rdata}

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
      loadStoreStep loadStoreState LoadStoreReq {job = wbOut.loadStoreJob, memResp = dResp, granted = wbOut.loadStoreGranted, squash = cmOut.squash}

    forwards = fromEx 1 :> fromEx 0 :> fromWb 1 :> fromWb 0 :> mulDivResp.forwarding :> loadStoreResp.forwarding :> Nil
      where
        fromEx, fromWb :: Index DispatchWidth -> Forwarding
        exLanes = Upto.toMaybes readyResp.rdata
        wbLanes = Upto.toMaybes executedResp.rdata
        fromEx i = F.forwarding (pdOf =<< exLanes !! i) $ do
          entry <- exLanes !! i
          guard exOut.issued
          orNothing (hasResult entry) (exOut.wbData !! i)
        fromWb i = F.forwarding (pdOf =<< wbLanes !! i) $ do
          entry <- wbLanes !! i
          orNothing (hasResult entry) entry.wbData

    redirect = cmOut.redirect <|> exOut.redirect
    flush = isJust redirect
    serializingInFlight' = not cmOut.squash && (serializingInFlight || wbOut.serializing)
    rsAddrs = concatMap (maybe (repeat 0) (\Renamed {..} -> ps1Addr :> ps2Addr :> Nil)) (Upto.toMaybes renamedResp.rdata)
    regReq = RegReq {rsAddrs, writes = (regWrite =<<) <$> wbOut.completions}
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}

    fetchedReq = RingReq {wdata = ifOut.issue, pop = idOut.issue.len, squash = flush}
    decodedReq = FifoReq {wdata = idOut.issue, rready = rnOut.issue.len > 0, flush}
    renamedReq = FifoReq {wdata = rnOut.issue, rready = rrOut.issue.len > 0, flush}
    readyReq = FifoReq {wdata = rrOut.issue, rready = exOut.issued, flush}
    executedReq = FifoReq {wdata = exOut.issue, rready = wbOut.issued, flush = isJust cmOut.redirect}
    robReq = RobReq {allocates = rnOut.allocates, completes = (robWrite =<<) <$> wbOut.completions, pop = cmOut.pop, squash = cmOut.squash}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = loadStoreResp.memReq, retired = cmOut.retired, led = csrFile.led, coreTrace}
    coreTrace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , ifIssue = if flush then Upto.empty else ifOut.issue
        , idIssue = idOut.issue.len
        , rnIssue = rnOut.issue.len
        , rrIssue = rrOut.issue.len
        , exIssue = exOut.issue.len
        , wbIssue = wbOut.issue
        , retired = cmOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', renameState = renameState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile', serializingInFlight = serializingInFlight'}
