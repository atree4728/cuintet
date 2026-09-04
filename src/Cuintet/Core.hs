-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, SSWay, XLen)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), MaWb (..), Retire (..), forwardable, unresolved)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), MemAccessState (..), initMemAccessState, memAccess)
import Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback)
import Cuintet.Unit.Btb (BtbReq (..), BtbResp, btb)
import Cuintet.Unit.Csr (CsrFile (led))
import Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.Unit.MulDiv (MulDivState (..))
import Cuintet.Unit.RegFile (RegReq (..), RegResp, mkRegReq, regFile)
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
  , retired :: Vec SSWay (Maybe Retire)
  -- ^ Execution log of a single instruction, emitted only in the clock it retires.
  , led :: BitVector XLen
  , trace :: CoreTrace
  }

-- | The core's state.
data CoreState = CoreState
  { fetchState :: FetchState
  , mulDivState :: MulDivState
  , memAccessState :: MemAccessState
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState = CoreState {fetchState = initFetchState, mulDivState = Idle, memAccessState = initMemAccessState}

data CoreTrace = CoreTrace
  { fetchStart :: Maybe Addr
  , fetchDone :: Bool
  , ifIssue :: Maybe IfId
  , idIssue :: Bool
  , exIssue :: Bool
  , maIssue :: Bool
  , retired :: Vec SSWay (Maybe Retire)
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
    (coreOut, regReq, btbReq, ifIdReq, idExReq, exMaReq, maWbReq) =
      mealyB coreT initState (coreIn, regResp, btbResp, ifIdResp, idExResp, exMaResp, maWbResp)
    btbResp = btb btbReq
    regResp = regFile regReq
    ifIdResp = fifo d3 ifIdReq
    idExResp = fifo d1 idExReq
    exMaResp = fifo d1 exMaReq
    maWbResp = fifo d1 maWbReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  (CoreIn, RegResp, BtbResp, FifoResp IfId, FifoResp IdEx, FifoResp ExMa, FifoResp MaWb) ->
  (CoreState, (CoreOut, RegReq, BtbReq, FifoReq IfId, FifoReq IdEx, FifoReq ExMa, FifoReq MaWb))
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, ifIdResp, idExResp, exMaResp, maWbResp) =
  (state', (coreOut, regReq, btbReq, ifIdReq, idExReq, exMaReq, maWbReq))
  where
    (fetchState', ifOut) = fetch fetchState FetchIn {iResp, fifo = ifIdResp, redirect = maOut.redirect, btbResp}
    idOut = decode DecodeIn {entry = ifIdResp.rdata, regResp, forwards, pending, wready = idExResp.wready, flush}
    (mulDivState', exOut) = execute mulDivState ExecuteIn {entry = idExResp.rdata, wready = exMaResp.wready}
    (memAccessState', maOut) = memAccess memAccessState MemAccessIn {entry = exMaResp.rdata, dResp}
    wbOut = writeback WriteBackIn {entry = maWbResp.rdata}

    pending = (unresolved =<< idExResp.rdata) :> (unresolved =<< exMaResp.rdata) :> Nil
    flush = isJust maOut.redirect
    forwards = (forwardable =<< exOut.issue) :> (forwardable =<< exMaResp.rdata) :> Nil

    regReq = mkRegReq ifIdResp.rdata $ wbOut.retired >>= (.rd)
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, write = maOut.btbWrite}

    ifIdReq = FifoReq {wdata = ifOut.issue, rready = isJust idOut.issue, flush}
    idExReq = FifoReq {wdata = idOut.issue, rready = isJust exOut.issue, flush}
    exMaReq = FifoReq {wdata = exOut.issue, rready = isJust maOut.issue, flush = False}
    maWbReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, retired = wbOut.retired :> Nil, led = memAccessState.csrFile.led, trace}
    trace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , fetchDone = isJust fetchState.fetching && isJust iResp.rdata && not flush
        , ifIssue = if ifIdResp.wready && not flush then ifOut.issue else Nothing
        , idIssue = isJust idOut.issue
        , exIssue = isJust exOut.issue
        , maIssue = isJust maOut.issue
        , retired = wbOut.retired :> Nil
        , flush
        }

    state' = CoreState {fetchState = fetchState', mulDivState = mulDivState', memAccessState = memAccessState'}
