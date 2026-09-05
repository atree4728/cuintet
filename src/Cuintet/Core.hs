-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, NLanes, XLen)
import Cuintet.Forwarding (forwarding)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), MaCm (..), Retire (..), destReg, hasResult, serializing)
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
import Cuintet.Unit.RegFile (RegReq (..), RegResp, mkRegReq, regFile)
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
  , retired :: Vec NLanes (Maybe Retire)
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
  , ifIssue :: Maybe IfId
  , idIssue :: Bool
  , exIssue :: Bool
  , maIssue :: Bool
  , retired :: Vec NLanes (Maybe Retire)
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
    ifIdResp = fifo d3 ifIdReq
    idExResp = fifo d1 idExReq
    exMaResp = fifo d1 exMaReq
    maCmResp = fifo d1 maCmReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  (CoreIn, RegResp, BtbResp, FifoResp IfId, FifoResp IdEx, FifoResp ExMa, FifoResp MaCm) ->
  (CoreState, (CoreOut, RegReq, BtbReq, FifoReq IfId, FifoReq IdEx, FifoReq ExMa, FifoReq MaCm))
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, ifIdResp, idExResp, exMaResp, maCmResp) =
  (state', (coreOut, regReq, btbReq, ifIdReq, idExReq, exMaReq, maCmReq))
  where
    serializingInFlight = maybe False serializing exMaResp.rdata || maybe False serializing maCmResp.rdata

    fetchIn = FetchIn {iResp, fifo = ifIdResp, redirect, btbResp}
    decodeIn = DecodeIn {entry = ifIdResp.rdata, regResp, forwards, wready = idExResp.wready, flush}
    executeIn = ExecuteIn {entry = idExResp.rdata, wready = exMaResp.wready, serializingInFlight}
    memAccessIn = MemAccessIn {entry = exMaResp.rdata, dResp}
    commitIn = CommitIn {entry = maCmResp.rdata}

    (fetchState', ifOut) = fetch fetchState fetchIn
    idOut = decode decodeIn
    (mulDivState', exOut) = execute mulDivState executeIn
    (loadStoreState', maOut) = memAccess loadStoreState memAccessIn
    (csrFile', cmOut) = commit csrFile commitIn

    forwards = forwarding (destReg =<< idExResp.rdata) fromEx :> forwarding (destReg =<< exMaResp.rdata) fromMa :> Nil
      where
        fromEx = do
          entry <- idExResp.rdata
          out <- exOut.issue
          orNothing (hasResult entry) out.wbData
        fromMa = do
          entry <- exMaResp.rdata
          orNothing (hasResult entry) entry.wbData
    redirect = cmOut.redirect <|> exOut.redirect
    flush = isJust redirect

    regReq = mkRegReq ifIdResp.rdata $ cmOut.write
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, write = exOut.btbWrite}

    ifIdReq = FifoReq {wdata = ifOut.issue, rready = isJust idOut.issue, flush}
    idExReq = FifoReq {wdata = idOut.issue, rready = isJust exOut.issue, flush}
    exMaReq = FifoReq {wdata = exOut.issue, rready = isJust maOut.issue, flush = False}
    maCmReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, retired = cmOut.retired, led = cmOut.led, coreTrace}
    coreTrace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , fetchDone = isJust fetchState.fetching && isJust iResp.rdata && not flush
        , ifIssue = if ifIdResp.wready && not flush then ifOut.issue else Nothing
        , idIssue = isJust idOut.issue
        , exIssue = isJust exOut.issue
        , maIssue = isJust maOut.issue
        , retired = cmOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile'}
