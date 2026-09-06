-- | The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own @Cuintet.Stage.*@ module.
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, NLanes, XLen)
import Cuintet.Forwarding (forwarding)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), IfIdDepth, MaCm (..), Retire (..), destReg, hasResult, serializing)
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
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
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
    ifIdResp = ring (SNat @IfIdDepth) ifIdReq
    idExResp = fifo idExReq
    exMaResp = fifo exMaReq
    maCmResp = fifo maCmReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  (CoreIn, RegResp, BtbResp, RingResp IfIdDepth NLanes IfId, FifoResp IdEx, FifoResp ExMa, FifoResp MaCm) ->
  (CoreState, (CoreOut, RegReq, BtbReq, RingReq 1 NLanes IfId, FifoReq IdEx, FifoReq ExMa, FifoReq MaCm))
coreT CoreState {..} (~CoreIn {..}, regResp, btbResp, ifIdResp, idExResp, exMaResp, maCmResp) =
  (state', (coreOut, regReq, btbReq, ifIdReq, idExReq, exMaReq, maCmReq))
  where
    serializingInFlight = maybe False serializing exMaResp.rdata || maybe False serializing maCmResp.rdata

    ifIdEntry = Upto.first ifIdResp.rdata

    fetchIn = FetchIn {iResp, buf = ifIdResp, redirect, btbResp}
    decodeIn = DecodeIn {entry = ifIdEntry, regResp, forwards, wready = idExResp.wready, flush}
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

    regReq = mkRegReq ifIdEntry $ cmOut.write
    btbReq = BtbReq {lookupAddr = ifOut.btbLookup, prefetchAddr = ifOut.btbPrefetch, write = exOut.btbWrite}

    ifIdReq = RingReq {wdata = ifOut.issue, pop = if isJust idOut.issue then 1 else 0, flush}
    idExReq = FifoReq {wdata = idOut.issue, rready = isJust exOut.issue, flush}
    exMaReq = FifoReq {wdata = exOut.issue, rready = isJust maOut.issue, flush = False}
    maCmReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, retired = cmOut.retired, led = cmOut.led, coreTrace}
    coreTrace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , fetchDone = isJust fetchState.fetching && isJust iResp.rdata && not flush
        , ifIssue = if flush then Nothing else Upto.first ifOut.issue
        , idIssue = isJust idOut.issue
        , exIssue = isJust exOut.issue
        , maIssue = isJust maOut.issue
        , retired = cmOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', mulDivState = mulDivState', loadStoreState = loadStoreState', csrFile = csrFile'}
