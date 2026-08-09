{- |
The core in terms of the classical IF, ID, EX, MA and WB stages. Only IF is
pipelined, against the rest, through the instruction FIFO; ID through WB form a
single combinational cone that handles one instruction at a time.

In the diagrams below, @[x]@ is a register, i.e. a clock boundary, and @(x)@ is
combinational logic.

@
    IF    [ifPc] --> iReq --> memory --> iResp --> [ifFifoWdata] --> [fifo]
                                                                       |
    ===================================================================|===
      the FIFO output is the only register boundary between the stages |
    ===================================================================|===
                                                                       |
    ID    (instDecode), [regFile] read, (operands) <--------------------+
            |
    EX    (alu), (brUnit), (csrUnitStep) --> [csrFile]
            |
    MA    (memUnitStep) --> [memUnitState] --> dReq
            |
    WB    [regFile] write, (control hazard) --> [ifPc] + FIFO flush
@

[IF]: Runs ahead on its own. Each of the three arrows out of a register takes a
  clock, so an instruction reaches the head of the FIFO three clocks after its
  request goes out, while the throughput stays at one instruction per clock.
  The FIFO absorbs the difference between that and the rate ID drains it at.

[ID]: Decoding and the register file read. Holds no state; @regFile@ is read
  combinationally out of the register bank that WB writes.

[EX]: The ALU, the branch condition, and the CSR access. @csrFile@ is the only
  thing here that carries over to the next clock.

[MA]: The one stage that takes more than a clock. @memUnitStep@ walks
  @memUnitState@ through @Idle@, @WaitReady@ and @WaitValid@, driving @dReq@
  from the register rather than straight from the ALU, and stalls ID through WB
  until the data comes back.

[WB]: The write back and the control hazard, which redirects @ifPc@ and flushes
  everything IF has fetched so far. An instruction commits on the clock where it
  is valid and MA does not stall; draining the FIFO and writing back both follow
  that condition.
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
  }

-- | Execution log of a single instruction, emitted only in the clock it commits.

-- | Core state. The @if@ prefix is short for instruction fetch.
data CoreState = CoreState
  { fetchState :: FetchState
  , regFile :: RegFile
  -- ^ Register file.
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

-- | Closes 'coreT' around the instruction FIFO.
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core coreIn = coreOut
  where
    (coreOut, instReq, idExReq, exMaReq, maWbReq) = unbundle $ mealy coreT initState (bundle (coreIn, instResp, idExResp, exMaResp, maWbResp))
    instResp = fifo d3 instReq
    idExResp = fifo d1 idExReq
    exMaResp = fifo d1 exMaReq
    maWbResp = fifo d1 maWbReq

-- | One clock of every stage, all combinational.
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

    inflights =
      (idExResp.rdata >>= pendingRd)
        :> (exMaResp.rdata >>= pendingRd)
        :> (maWbResp.rdata >>= pendingRd)
        :> Nil

    ifIdReq = FifoReq {wdata = ifOut.issue, rready = isJust idOut.issue, flush = isJust maOut.redirect}
    idExReq = FifoReq {wdata = idOut.issue, rready = isJust exOut.issue, flush = isJust maOut.redirect}
    exMaReq = FifoReq {wdata = exOut.issue, rready = isJust maOut.issue, flush = False}
    maWbReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}

    coreOut = CoreOut {iReq = ifOut.iReq, dReq = maOut.dReq, instLog = wbOut.retired}

    state' = CoreState {fetchState = fetchState', regFile = regFile', memAccessState = memAccessState'}
