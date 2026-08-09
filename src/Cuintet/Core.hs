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
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, RegFile, instAt)
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), MaWb (..), pendingRd)
import Cuintet.Stage.Decode (decode, hazard)
import Cuintet.Stage.Execute (execute)
import Cuintet.Stage.MemAccess (MemAccessIn (..), MemAccessOut (..), MemAccessState (..), initMemAccessState, memAccess)
import Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), initRegFile, writeback)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

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
  { next :: Addr
  -- ^ Program counter.
  , fetching :: Maybe Addr
  -- ^ Address being fetched.
  , staged :: Maybe IfId
  -- ^ The instruction waiting to be written.
  , regFile :: RegFile
  -- ^ Register file.
  , memAccessState :: MemAccessState
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState =
  CoreState
    { next = 0
    , fetching = Nothing
    , staged = Nothing
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
coreT CoreState {..} (~CoreIn {iResp, dResp}, instResp, idExResp, exMaResp, maWbResp) = (state', (coreOut, instReq, idExReq, exMaReq, maWbReq))
  where
    inflights =
      (idExResp.rdata >>= pendingRd)
        :> (exMaResp.rdata >>= pendingRd)
        :> (maWbResp.rdata >>= pendingRd)
        :> Nil

    (memAccessState', maOut) = memAccess memAccessState MemAccessIn {entry = exMaResp.rdata, dResp}

    -- IF stage
    instReq = FifoReq {wdata = staged, rready = idIssue, flush = controlHazard}

    -- ID stage
    idValid = isJust instResp.rdata
    idEx = decode regFile $ fromMaybe (deepErrorX "coreT: IF-ID FIFO is empty") instResp.rdata
    idIssue = idValid && not (hazard idEx inflights) && idExResp.wready && not controlHazard
    idExReq = FifoReq {wdata = orNothing idIssue idEx, rready = exIssue, flush = controlHazard}

    -- EX-WB stage
    exValid = isJust idExResp.rdata
    exMa = execute $ fromMaybe (deepErrorX "coreT: ID-EX FIFO is empty") idExResp.rdata
    exIssue = exValid && exMaResp.wready

    exMaReq = FifoReq {wdata = orNothing exIssue exMa, rready = isJust maOut.issue, flush = False}

    maWbReq = FifoReq {wdata = maOut.issue, rready = True, flush = False}
    controlHazard = isJust maOut.redirect

    (regFile', wbOut) = writeback regFile WriteBackIn {entry = maWbResp.rdata}
    -- IF: the answer to the request issued when @ifRequested@ was set.
    fetched = (,) <$> fetching <*> iResp.rdata
    accepted = instResp.wreadyTwo && iResp.ready

    -- next state
    (next', fetching')
      | Just target <- maOut.redirect = (target, Nothing)
      | accepted = (next + 4, Just next)
      | otherwise = (next, fetching)
    staged'
      | controlHazard = Nothing
      | Just (addr, busWord) <- fetched = Just IfId {pc = addr, instBits = instAt addr busWord}
      | instResp.wready = Nothing
      | otherwise = staged -- pending

    -- Neither request may depend combinationally on @iResp@ or @dResp@, or a
    -- combinational loop closes through @memArbiter@. Both are driven straight
    -- from registers: @iReq@ from @ifPc@ and the FIFO, @dReq@ from
    -- @memUnitState@. A fetch goes out only when the FIFO has room for the
    -- pending write and this one both.
    coreOut =
      CoreOut
        { iReq = orNothing instResp.wreadyTwo BusReq {addr = next, wdata = Nothing}
        , dReq = maOut.dReq
        , instLog = wbOut.retired
        }

    state' =
      CoreState
        { next = next'
        , fetching = fetching'
        , staged = staged'
        , regFile = regFile'
        , memAccessState = memAccessState'
        }
