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
    ID|   (decode), [regFile] read, (interlock) <----+
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
    WB    [regFile] write --> instLog
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
-}
module Cuintet.Core (CoreIn (..), CoreOut (..), core) where

import Clash.Prelude
import Cuintet.Eei (MemReq, MemResp, RegFile)
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), MaWb (..), pendingRd)
import Cuintet.Stage.Decode (DecodeIn (..), DecodeOut (..), decode)
import Cuintet.Stage.Execute (ExecuteIn (..), ExecuteOut (..), execute)
import Cuintet.Stage.Fetch (FetchIn (..), FetchOut (..), FetchState (..), fetch, initFetchState)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), MemAccessState (..), initMemAccessState, memAccess)
import Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), initRegFile, writeback)
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
  }

{- | The core's registers. Each stage owns its own, except @regFile@, which WB
writes and ID reads.
-}
data CoreState = CoreState
  { fetchState :: FetchState
  , regFile :: RegFile
  , memAccessState :: MemAccessState
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState =
  CoreState
    { fetchState = initFetchState
    , regFile = initRegFile
    , memAccessState = initMemAccessState
    }

{- | Closes 'coreT' around the four stage FIFOs. IF-ID is the deep one, since it
is what lets IF run ahead; the rest only need to hold a single instruction.
-}
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core coreIn = coreOut
  where
    (coreOut, ifIdReq, idExReq, exMaReq, maWbReq) = unbundle $ mealy coreT initState (bundle (coreIn, ifIdResp, idExResp, exMaResp, maWbResp))
    ifIdResp = fifo d3 ifIdReq
    idExResp = fifo d1 idExReq
    exMaResp = fifo d1 exMaReq
    maWbResp = fifo d1 maWbReq

-- | One clock of every stage.
coreT ::
  CoreState ->
  (CoreIn, FifoResp IfId, FifoResp IdEx, FifoResp ExMa, FifoResp MaWb) ->
  (CoreState, (CoreOut, FifoReq IfId, FifoReq IdEx, FifoReq ExMa, FifoReq MaWb))
coreT CoreState {..} (~CoreIn {iResp, dResp}, ifIdResp, idExResp, exMaResp, maWbResp) = (state', (coreOut, ifIdReq, idExReq, exMaReq, maWbReq))
  where
    (fetchState', ifOut) = fetch fetchState FetchIn {iResp, fifo = ifIdResp, redirect = maOut.redirect}
    idOut = decode DecodeIn {entry = ifIdResp.rdata, regFile, inflights, wready = idExResp.wready, flush = isJust maOut.redirect}
    exOut = execute ExecuteIn {entry = idExResp.rdata, wready = exMaResp.wready}
    (memAccessState', maOut) = memAccess memAccessState MemAccessIn {entry = exMaResp.rdata, dResp}
    (regFile', wbOut) = writeback regFile WriteBackIn {entry = maWbResp.rdata}

    -- what the instructions downstream of ID will write back
    inflights =
      (idExResp.rdata >>= pendingRd)
        :> (exMaResp.rdata >>= pendingRd)
        :> (maWbResp.rdata >>= pendingRd)
        :> Nil

    -- a stage consumes its input exactly on the clock it produces an output
    ifIdReq = FifoReq {wdata = ifOut.issue, rready = isJust idOut.issue, flush = isJust maOut.redirect}
    idExReq = FifoReq {wdata = idOut.issue, rready = isJust exOut.issue, flush = isJust maOut.redirect}
    exMaReq = FifoReq {wdata = exOut.issue, rready = isJust maOut.issue, flush = False}
    maWbReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, instLog = wbOut.retired}

    state' = CoreState {fetchState = fetchState', regFile = regFile', memAccessState = memAccessState'}
