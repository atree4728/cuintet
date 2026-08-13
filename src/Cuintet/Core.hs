{- |
The core in terms of the classical IF, ID, EX, MA and WB stages, each in its own
@Cuintet.Stage.*@ module. This module is the wiring: it holds the registers,
calls the five stages, and drives the four FIFOs between them.

In the diagram below, @[x]@ is a register, i.e. a clock boundary, and @(x)@ is
combinational logic.

@
    IF    [next] --> iReq --> memory --> iResp --> [staged]
      ^                                              |
      |                                        [IF-ID FIFO]
      |                                              |
    ID|   (decode), regFile read, (interlock) <------+
      |     |
      |   [ID-EX FIFO]
      |     |
    EX|   (operands), (alu), (branchUnit)
      |     |
      |   [EX-MA FIFO]
      |     |
    MA|   (csrStep) --> [csrFile]
      |   (loadStoreStep) --> [loadStoreState] --> dReq
      +-- (redirect), which also flushes the IF-ID and ID-EX FIFOs
            |
          [MA-WB FIFO]
            |
    WB    regFile write --> instLog
@

Every stage consumes its input exactly on the clock it produces an output, so a
FIFO's @rready@ is the @isJust@ of the next stage's @issue@ and no stage needs to
be told about a stall further down. Only the IF-ID and ID-EX FIFOs are flushed on
a redirect: the EX-MA and MA-WB FIFOs hold instructions at least as old as the
one that redirected, including that instruction itself, and all of them must
still retire.

Neither @iReq@ nor @dReq@ may depend combinationally on @iResp@ or @dResp@, or a
combinational loop closes through 'Cuintet.BusArbiter.busArbiter'. Both are
driven out of registers, @iReq@ from IF's and @dReq@ from MA's.

[IF]: 'Cuintet.Stage.Fetch.fetch'. Runs ahead on its own; the IF-ID FIFO absorbs
  the difference between its rate and the rate ID drains it at.

[ID]: 'Cuintet.Stage.Decode.decode'. Holds no state. It stalls itself by not
  issuing when the instruction reads a register that an instruction already
  downstream will write, so a flush needs no rollback.

[EX]: 'Cuintet.Stage.Execute.execute'. A pure function.

[MA]: 'Cuintet.Stage.MemAccess.memAccess'. The one stage that can take more than
  a clock. It owns @csrFile@ and the load\/store unit's state, and it is where
  control flow is resolved.

[WB]: 'Cuintet.Stage.Writeback.writeback'. Never stalls, which is what lets MA
  start an access without checking the MA-WB FIFO for room.

[regFile]: 'Cuintet.RegFile.regFile'. The one piece of state that is not in
  'CoreState', because it is a RAM rather than a register. It is read
  asynchronously, addressed straight off the IF-ID FIFO head so the read
  overlaps decoding, and WB's write takes effect at the next clock edge. The
  interlock in ID is what keeps a reader from getting ahead of that write.
-}
module Cuintet.Core (CoreIn (..), CoreOut (..), CoreTrace (..), core) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, XLen)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), MaWb (..), forwardable, unresolved)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), MemAccessState (..), initMemAccessState, memAccess)
import Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback)
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
  , instLog :: Maybe MaWb
  -- ^ Execution log of a single instruction, emitted only in the clock it retires.
  , led :: BitVector XLen
  , trace :: CoreTrace
  }

-- | The core's registers, one per stage that has any. The register file is not here; see 'Cuintet.RegFile.regFile'.
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
  , instLog :: Maybe MaWb
  , flush :: Bool
  }
  deriving (Generic, NFDataX)

-- | Closes 'coreT' around the register file and the four stage FIFOs. IF-ID is the deep one, since it is what lets IF run ahead; the rest only need to hold a single instruction.
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core coreIn = coreOut
  where
    (coreOut, regReq, ifIdReq, idExReq, exMaReq, maWbReq) = mealyB coreT initState (coreIn, regResp, ifIdResp, idExResp, exMaResp, maWbResp)
    regResp = regFile regReq
    ifIdResp = fifo d3 ifIdReq
    idExResp = fifo d1 idExReq
    exMaResp = fifo d1 exMaReq
    maWbResp = fifo d1 maWbReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  (CoreIn, RegResp, FifoResp IfId, FifoResp IdEx, FifoResp ExMa, FifoResp MaWb) ->
  (CoreState, (CoreOut, RegReq, FifoReq IfId, FifoReq IdEx, FifoReq ExMa, FifoReq MaWb))
coreT CoreState {..} (~CoreIn {..}, regResp, ifIdResp, idExResp, exMaResp, maWbResp) =
  (state', (coreOut, regReq, ifIdReq, idExReq, exMaReq, maWbReq))
  where
    (fetchState', ifOut) = fetch fetchState FetchIn {iResp, fifo = ifIdResp, redirect = maOut.redirect}
    idOut = decode DecodeIn {entry = ifIdResp.rdata, regResp, forwards, pending, wready = idExResp.wready, flush}
    (mulDivState', exOut) = execute mulDivState ExecuteIn {entry = idExResp.rdata, wready = exMaResp.wready}
    (memAccessState', maOut) = memAccess memAccessState MemAccessIn {entry = exMaResp.rdata, dResp}
    wbOut = writeback WriteBackIn {entry = maWbResp.rdata}

    pending = (unresolved =<< idExResp.rdata) :> (unresolved =<< exMaResp.rdata) :> Nil
    flush = isJust maOut.redirect
    forwards = (forwardable =<< exOut.issue) :> (forwardable =<< exMaResp.rdata) :> Nil

    -- read for whatever ID is about to decode, write for whatever WB has just retired
    regReq = mkRegReq ifIdResp.rdata wbOut.write

    -- a stage consumes its input exactly on the clock it produces an output
    ifIdReq = FifoReq {wdata = ifOut.issue, rready = isJust idOut.issue, flush}
    idExReq = FifoReq {wdata = idOut.issue, rready = isJust exOut.issue, flush}
    exMaReq = FifoReq {wdata = exOut.issue, rready = isJust maOut.issue, flush = False}
    maWbReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, instLog = wbOut.retired, led = memAccessState.csrFile.led, trace}
    trace =
      CoreTrace
        { fetchStart = if iResp.ready && not flush then (.addr) <$> ifOut.iReq else Nothing
        , fetchDone = isJust fetchState.fetching && isJust iResp.rdata && not flush
        , ifIssue = if ifIdResp.wready && not flush then ifOut.issue else Nothing
        , idIssue = isJust idOut.issue
        , exIssue = isJust exOut.issue
        , maIssue = isJust maOut.issue
        , instLog = wbOut.retired
        , flush
        }

    state' = CoreState {fetchState = fetchState', mulDivState = mulDivState', memAccessState = memAccessState'}
