-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), FetchWidth, IssueWidth, MemReq, MemResp, XLen)
import Cuintet.Forwarding (Forwarding, forwarding)
import Cuintet.Pipeline (Completed (..), Decoded (..), Executed (..), FetchBufBits, Fetched (..), Ready (..), Renamed (..), Retire (..), hasResult, pdOf, serializing)
import Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), memAccess)
import Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead)
import Cuintet.Stage.Rename (RenameIn (..), RenameOut (..), RenameState, initRenameState, rename)
import Cuintet.Unit.Btb (BtbReq (..), BtbResp, btb)
import Cuintet.Unit.Csr (CsrFile, initCsrFile)
import Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.Unit.LoadStore (LoadStoreState)
import Cuintet.Unit.LoadStore qualified as L
import Cuintet.Unit.MulDiv (MulDivState)
import Cuintet.Unit.MulDiv qualified as M
import Cuintet.Unit.RegFile (RegReq (..), RegResp (..), regFile)
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
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
  , retired :: Vec IssueWidth (Maybe Retire)
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
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState = CoreState {fetchState = initFetchState, renameState = initRenameState, mulDivState = M.Idle, loadStoreState = L.Idle, csrFile = initCsrFile}

data CoreTrace = CoreTrace
  { fetchStart :: Maybe Addr
  , ifIssue :: Upto FetchWidth Fetched
  , idIssue :: Index (IssueWidth + 1)
  , rnIssue :: Index (IssueWidth + 1)
  , rrIssue :: Index (IssueWidth + 1)
  , exIssue :: Index (IssueWidth + 1)
  , maIssue :: Index (IssueWidth + 1)
  , retired :: Vec IssueWidth (Maybe Retire)
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
    (coreOut, regReq, btbReq, fetchedReq, decodedReq, renamedReq, readyReq, executedReq, completedReq) =
      mealyB coreT initState (coreIn, regResp, btbResp, fetchedResp, decodedResp, renamedResp, readyResp, executedResp, completedResp)
    btbResp = btb btbReq
    regResp = regFile regReq
    fetchedResp = ring (SNat @FetchBufBits) fetchedReq
    decodedResp = fifo decodedReq
    renamedResp = fifo renamedReq
    readyResp = fifo readyReq
    executedResp = fifo executedReq
    completedResp = fifo completedReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  ( CoreIn
  , RegResp
  , BtbResp
  , RingResp FetchBufBits IssueWidth Fetched
  , FifoResp IssueWidth Decoded
  , FifoResp IssueWidth Renamed
  , FifoResp IssueWidth Ready
  , FifoResp IssueWidth Executed
  , FifoResp IssueWidth Completed
  ) ->
  ( CoreState
  , ( CoreOut
    , RegReq
    , BtbReq
    , RingReq FetchWidth IssueWidth Fetched
    , FifoReq IssueWidth Decoded
    , FifoReq IssueWidth Renamed
    , FifoReq IssueWidth Ready
    , FifoReq IssueWidth Executed
    , FifoReq IssueWidth Completed
    )
  )
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, fetchedResp, decodedResp, renamedResp, readyResp, executedResp, completedResp) =
  (state', (coreOut, regReq, btbReq, fetchedReq, decodedReq, renamedReq, readyReq, executedReq, completedReq))
  where
    serializingInFlight = any serializing (Upto.head executedResp.rdata) || any serializing (Upto.head completedResp.rdata)

    fetchIn = FetchIn {iResp, buf = fetchedResp, redirect, btbResp}
    decodeIn = DecodeIn {entries = fetchedResp.rdata, wready = decodedResp.wready, stall = flush}
    renameIn = RenameIn {entries = decodedResp.rdata, committed = cmOut.renamed, flush, drained = executedResp.rdata.len == 0 && completedResp.rdata.len == 0, wready = renamedResp.wready}
    regReadIn = RegReadIn {entries = renamedResp.rdata, rsData = regResp.rsData, forwards, wready = readyResp.wready}
    executeIn = ExecuteIn {entries = readyResp.rdata, wready = executedResp.wready, serializingInFlight}
    memAccessIn = MemAccessIn {entries = executedResp.rdata, dResp}
    commitIn = CommitIn {entries = completedResp.rdata}

    (fetchState', ifOut) = fetch fetchState fetchIn
    idOut = decode decodeIn
    (renameState', rnOut) = rename renameState renameIn
    rrOut = regRead regReadIn
    (mulDivState', exOut) = execute mulDivState executeIn
    (loadStoreState', maOut) = memAccess loadStoreState memAccessIn
    (csrFile', cmOut) = commit csrFile commitIn

    forwards = fromEx 1 :> fromEx 0 :> fromMa 1 :> fromMa 0 :> Nil
      where
        fromEx, fromMa :: Index IssueWidth -> Forwarding
        exLanes = Upto.toMaybes readyResp.rdata
        maLanes = Upto.toMaybes executedResp.rdata
        fromEx i = forwarding (pdOf =<< exLanes !! i) $ do
          entry <- exLanes !! i
          guard exOut.issued
          orNothing (hasResult entry) (exOut.wbData !! i)
        fromMa i = forwarding (pdOf =<< maLanes !! i) $ do
          entry <- maLanes !! i
          orNothing (hasResult entry) entry.wbData

    redirect = cmOut.redirect <|> exOut.redirect
    flush = isJust redirect

    regReq =
      RegReq
        { rsAddrs = concatMap (maybe (repeat 0) (\d -> d.rs1Addr :> d.rs2Addr :> Nil)) (Upto.toMaybes renamedResp.rdata)
        , writes = (>>= (.rd)) <$> cmOut.retired
        }
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}

    fetchedReq = RingReq {wdata = ifOut.issue, pop = idOut.issue.len, flush}
    decodedReq = FifoReq {wdata = idOut.issue, rready = rnOut.issue.len > 0, flush}
    renamedReq = FifoReq {wdata = rnOut.issue, rready = rrOut.issue.len > 0, flush}
    readyReq = FifoReq {wdata = rrOut.issue, rready = exOut.issued, flush}
    executedReq = FifoReq {wdata = exOut.issue, rready = maOut.issued, flush = False}
    completedReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, retired = cmOut.retired, led = cmOut.led, coreTrace}
    coreTrace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , ifIssue = if flush then Upto.empty else ifOut.issue
        , idIssue = idOut.issue.len
        , rnIssue = rnOut.issue.len
        , rrIssue = rrOut.issue.len
        , exIssue = exOut.issue.len
        , maIssue = maOut.issue.len
        , retired = cmOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', renameState = renameState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile'}
