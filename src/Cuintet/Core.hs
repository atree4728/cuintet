-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), FetchWidth, IssueWidth, MemReq, MemResp, XLen)
import Cuintet.Forwarding (Forwarding, forwarding)
import Cuintet.Pipeline (Decoded (..), Executed (..), FetchBufBits, Fetched (..), Ready (..), Renamed (..), Retire (..), hasResult, pdOf)
import Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), memAccess)
import Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead)
import Cuintet.Stage.Rename (RenameIn (..), RenameOut (..), RenameState, initRenameState, rename)
import Cuintet.Unit.Btb (BtbReq (..), BtbResp, btb)
import Cuintet.Unit.Csr (CsrFile (led), initCsrFile)
import Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.Unit.LoadStore (LoadStoreState)
import Cuintet.Unit.LoadStore qualified as L
import Cuintet.Unit.MulDiv (MulDivState)
import Cuintet.Unit.MulDiv qualified as M
import Cuintet.Unit.RegFile (RegReq (..), RegResp (..), regFile)
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
import Cuintet.Unit.Rob (RobReq (..), RobResp (..), rob, usesCsrFile)
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
  , RingResp FetchBufBits IssueWidth Fetched
  , FifoResp IssueWidth Decoded
  , FifoResp IssueWidth Renamed
  , FifoResp IssueWidth Ready
  , FifoResp IssueWidth Executed
  ) ->
  ( CoreState
  , ( CoreOut
    , RegReq
    , BtbReq
    , RobReq
    , RingReq FetchBufBits FetchWidth IssueWidth Fetched
    , FifoReq IssueWidth Decoded
    , FifoReq IssueWidth Renamed
    , FifoReq IssueWidth Ready
    , FifoReq IssueWidth Executed
    )
  )
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, robResp, fetchedResp, decodedResp, renamedResp, readyResp, executedResp) =
  (state', (coreOut, regReq, btbReq, robReq, fetchedReq, decodedReq, renamedReq, readyReq, executedReq))
  where
    fetchIn = FetchIn {iResp, buf = fetchedResp, redirect, btbResp}
    decodeIn = DecodeIn {entries = fetchedResp.rdata, wready = decodedResp.wready, stall = flush}
    renameIn = RenameIn {entries = decodedResp.rdata, committed = cmOut.renamed, nextRobAddr = robResp.buffer.tl, robFree = robResp.buffer.free, flush, drained = robResp.buffer.rdata.len == 0, wready = renamedResp.wready}
    regReadIn = RegReadIn {entries = renamedResp.rdata, rsData = regResp.rsData, forwards, wready = readyResp.wready}
    executeIn = ExecuteIn {entries = readyResp.rdata, wready = executedResp.wready}
    memAccessIn = MemAccessIn {entries = executedResp.rdata, dResp, commitUsesCsrFile = any usesCsrFile (Upto.head robResp.buffer.rdata)}
    commitIn = CommitIn {entries = robResp.buffer.rdata}

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

    rsAddrs = concatMap (maybe (repeat 0) (\Renamed {..} -> ps1Addr :> ps2Addr :> Nil)) (Upto.toMaybes renamedResp.rdata)
    regReq = RegReq {rsAddrs, writes = maybe maOut.writes (\w -> Just w :> Nothing :> Nil) cmOut.write}
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}

    fetchedReq = RingReq {wdata = ifOut.issue, pop = idOut.issue.len, squash = flush}
    decodedReq = FifoReq {wdata = idOut.issue, rready = rnOut.issue.len > 0, flush}
    renamedReq = FifoReq {wdata = rnOut.issue, rready = rrOut.issue.len > 0, flush}
    readyReq = FifoReq {wdata = rrOut.issue, rready = exOut.issued, flush}
    executedReq = FifoReq {wdata = exOut.issue, rready = maOut.issued, flush = isJust cmOut.redirect}
    robReq = RobReq {allocates = rnOut.allocates, completes = maOut.issue, pop = cmOut.pop, squash = cmOut.squash}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, retired = cmOut.retired, led = csrFile.led, coreTrace}
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
