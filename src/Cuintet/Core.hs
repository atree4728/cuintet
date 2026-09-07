-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), FetchWidth, IssueWidth, MemReq, MemResp, XLen)
import Cuintet.Forwarding (Forwarding, forwarding)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), IfIdBits, MaCm (..), Retire (..), destReg, hasResult, serializing, srcAddrs)
import Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), memAccess)
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
  , mulDivState :: MulDivState
  , loadStoreState :: LoadStoreState
  , csrFile :: CsrFile
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState = CoreState {fetchState = initFetchState, mulDivState = M.Idle, loadStoreState = L.Idle, csrFile = initCsrFile}

data CoreTrace = CoreTrace
  { fetchStart :: Maybe Addr
  , fetchDone :: Bool
  , ifIssue :: Upto FetchWidth IfId
  , idIssue :: Index (IssueWidth + 1)
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
    (coreOut, regReq, btbReq, ifIdReq, idExReq, exMaReq, maCmReq) =
      mealyB coreT initState (coreIn, regResp, btbResp, ifIdResp, idExResp, exMaResp, maCmResp)
    btbResp = btb btbReq
    regResp = regFile regReq
    ifIdResp = ring (SNat @IfIdBits) ifIdReq
    idExResp = fifo idExReq
    exMaResp = fifo exMaReq
    maCmResp = fifo maCmReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  ( CoreIn
  , RegResp
  , BtbResp
  , RingResp IfIdBits IssueWidth IfId
  , FifoResp (Upto IssueWidth IdEx)
  , FifoResp (Upto IssueWidth ExMa)
  , FifoResp (Upto IssueWidth MaCm)
  ) ->
  ( CoreState
  , ( CoreOut
    , RegReq
    , BtbReq
    , RingReq FetchWidth IssueWidth IfId
    , FifoReq (Upto IssueWidth IdEx)
    , FifoReq (Upto IssueWidth ExMa)
    , FifoReq (Upto IssueWidth MaCm)
    )
  )
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, ifIdResp, idExResp, exMaResp, maCmResp) =
  (state', (coreOut, regReq, btbReq, ifIdReq, idExReq, exMaReq, maCmReq))
  where
    exHeld = Upto.held idExResp.rdata
    maHeld = Upto.held exMaResp.rdata
    cmHeld = Upto.held maCmResp.rdata

    serializingInFlight = any serializing (Upto.first maHeld) || any serializing (Upto.first cmHeld)

    fetchIn = FetchIn {iResp, buf = ifIdResp, redirect, btbResp}
    decodeIn = DecodeIn {entries = ifIdResp.rdata, rsData = regResp.rsData, forwards, wready = idExResp.wready, flush}
    executeIn = ExecuteIn {entries = exHeld, wready = exMaResp.wready, serializingInFlight}
    memAccessIn = MemAccessIn {entries = maHeld, dResp}
    commitIn = CommitIn {entries = cmHeld}

    (fetchState', ifOut) = fetch fetchState fetchIn
    idOut = decode decodeIn
    (mulDivState', exOut) = execute mulDivState executeIn
    (loadStoreState', maOut) = memAccess loadStoreState memAccessIn
    (csrFile', cmOut) = commit csrFile commitIn

    forwards = fromEx 1 :> fromEx 0 :> fromMa 1 :> fromMa 0 :> Nil
      where
        fromEx, fromMa :: Index IssueWidth -> Forwarding
        exLanes = Upto.toMaybes exHeld
        maLanes = Upto.toMaybes maHeld
        fromEx i = forwarding (destReg =<< exLanes !! i) $ do
          entry <- exLanes !! i
          guard exOut.issued
          orNothing (hasResult entry) (exOut.wbData !! i)
        fromMa i = forwarding (destReg =<< maLanes !! i) $ do
          entry <- maLanes !! i
          orNothing (hasResult entry) entry.wbData

    redirect = cmOut.redirect <|> exOut.redirect
    flush = isJust redirect

    regReq =
      RegReq
        { rsAddrs = concatMap srcAddrs (Upto.toMaybes ifIdResp.rdata)
        , writes = (>>= (.rd)) <$> cmOut.retired
        }
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, writes = exOut.btbWrites}

    ifIdReq = RingReq {wdata = ifOut.issue, pop = idOut.issue.len, flush}
    idExReq = FifoReq {wdata = orNothing (idOut.issue.len > 0) idOut.issue, rready = exOut.issued, flush}
    exMaReq = FifoReq {wdata = orNothing (exOut.issue.len > 0) exOut.issue, rready = maOut.issued, flush = False}
    maCmReq = FifoReq {wdata = orNothing (maOut.issue.len > 0) maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, retired = cmOut.retired, led = cmOut.led, coreTrace}
    coreTrace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , fetchDone = isJust fetchState.fetching && isJust iResp.rdata && not flush
        , ifIssue = if flush then Upto.none else ifOut.issue
        , idIssue = idOut.issue.len
        , exIssue = exOut.issue.len
        , maIssue = maOut.issue.len
        , retired = cmOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile'}
