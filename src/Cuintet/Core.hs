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
import Cuintet.CoreCtrl (InstCtrl (..), isBranchOp)
import Cuintet.CsrUnit (CsrAccess (..), CsrAddr (..), CsrFile, CsrReq (..), CsrResp (..), CsrTrap (..), csrUnitStep, initCsrFile, pattern ENVIRONMENT_CALL)
import Cuintet.Eei (Addr, MemBusReq (MemBusReq, addr, wdata), MemBusResp (rdata, ready), MemReq, MemResp, SystemOp (..), XLen, instAt)
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.MemUnit (InstInfo (..), MemUnitReq (..), MemUnitResp (..), MemUnitState (..), memUnitStep)
import Cuintet.Pipeline (ExMa (..), IdEx (..), IfId (..), MaWb (..), exMaRd, idExRd, maWbRd)
import Cuintet.Stage.Decode (decode, hazard)
import Cuintet.Stage.Execute (execute)
import Cuintet.Stage.Writeback (writeback)
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
  { ifPc :: Addr
  -- ^ Program counter.
  , ifRequested :: Maybe Addr
  -- ^ Address being fetched.
  , ifFifoWdata :: Maybe IfId
  -- ^ The instruction waiting to be written.
  , regFile :: Vec 32 (BitVector XLen)
  -- ^ Register file.
  , csrFile :: CsrFile
  -- ^ CSR file.
  , memUnitState :: MemUnitState
  -- ^ Memory unit state.
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState =
  CoreState
    { ifPc = 0
    , ifRequested = Nothing
    , ifFifoWdata = Nothing
    , regFile = zeroBits :> replicate d31 (deepErrorX "register uninitialized")
    , csrFile = initCsrFile
    , memUnitState = Idle
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
    pending = idExRd idExResp.rdata :> exMaRd exMaResp.rdata :> maWbRd maWbResp.rdata :> Nil

    -- IF stage
    instReq = FifoReq {wdata = ifFifoWdata, rready = idIssue, flush = controlHazard}

    -- ID stage
    idValid = isJust instResp.rdata
    idEx = decode regFile $ fromMaybe (deepErrorX "coreT: IF-ID FIFO is empty") instResp.rdata
    idIssue = idValid && not (hazard idEx pending) && idExResp.wready && not controlHazard
    idExReq = FifoReq {wdata = orNothing idIssue idEx, rready = exIssue, flush = controlHazard}

    -- EX-WB stage
    exValid = isJust idExResp.rdata
    exMa = execute $ fromMaybe (deepErrorX "coreT: ID-EX FIFO is empty") idExResp.rdata
    exIssue = exValid && exMaResp.wready

    maValid = isJust exMaResp.rdata
    ExMa {..} = fromMaybe (deepErrorX "coreT: EX-MA FIFO is empty") exMaResp.rdata
    exMaReq = FifoReq {wdata = orNothing exIssue exMa, rready, flush = False}

    -- EX: the CSR access, and the redirect that @ECALL@ and @MRET@ answer with.
    -- A system instruction never stalls, so the request is raised in the clock
    -- it commits and @csrFile@ is never updated twice.
    (csrFile', csrResp) = maybe (csrFile, Nothing) (fmap Just . csrUnitStep csrFile) csrReq
    csrReq
      | not maValid = Nothing
      | Just (SysCsr csrOp) <- ctrl.systemOp =
          Just $ Access CsrAccess {csrAddr = CsrAddr (slice d11 d0 imm), csrOp, rs1Addr, rs1Data}
      | Just SysEcall <- ctrl.systemOp = Just $ Trap CsrTrap {pc, mcause = ENVIRONMENT_CALL}
      | Just SysMret <- ctrl.systemOp = Just Mret
      | otherwise = Nothing
    csrRdata = case csrResp of Just (Accessed v) -> Just v; _ -> Nothing
    csrRedirect = case csrResp of Just (Redirect a) -> Just a; _ -> Nothing

    -- MA
    (memUnitState', memUnitResp) =
      memUnitStep
        memUnitState
        MemUnitReq
          { inst = orNothing maValid InstInfo {ctrl, addr = bitCoerce aluResult, wdata = rs2Data}
          , memResp = dResp
          }

    -- WB: the instruction leaves once MA has let go of it, draining the FIFO.
    rready = not memUnitResp.stall
    commit = maValid && rready

    wbData'
      | ctrl.isLoad = fromMaybe (deepErrorX "coreT: load committed without data") memUnitResp.rdata
      | Just rdata <- csrRdata = rdata
      | otherwise = wbData
    maWb =
      MaWb
        { pc
        , inst = instBits
        , ctrl
        , imm
        , rs1Addr
        , rs2Addr
        , rs1Data
        , rs2Data
        , op1
        , op2
        , aluResult
        , branchTaken = orNothing (isBranchOp ctrl) branchTaken
        , wbReq = orNothing (ctrl.rwbEn && rdAddr /= 0) (rdAddr, wbData')
        , csrRdata
        }

    maWbReq = FifoReq {wdata = orNothing commit maWb, rready = True, flush = False}

    -- Everything that redirects IF.
    controlHazard = commit && (isJust csrRedirect || ctrl.isJump || isBranchOp ctrl && branchTaken)

    -- IF: the answer to the request issued when @ifRequested@ was set.
    fetched = (,) <$> ifRequested <*> iResp.rdata
    accepted = instResp.wreadyTwo && iResp.ready

    -- next state
    (ifPc', ifRequested')
      | controlHazard, Just redirect <- csrRedirect = (redirect, Nothing)
      | controlHazard && ctrl.isJump = (bitCoerce $ aluResult .&. complement 1, Nothing) -- aluResult is the destination
      | controlHazard && isBranchOp ctrl = (pc + numConvert imm, Nothing)
      | accepted = (ifPc + 4, Just ifPc)
      | otherwise = (ifPc, ifRequested)
    ifFifoWdata'
      | controlHazard = Nothing
      | Just (addr, busWord) <- fetched = Just IfId {pc = addr, instBits = instAt addr busWord}
      | instResp.wready = Nothing
      | otherwise = ifFifoWdata -- pending

    -- Neither request may depend combinationally on @iResp@ or @dResp@, or a
    -- combinational loop closes through @memArbiter@. Both are driven straight
    -- from registers: @iReq@ from @ifPc@ and the FIFO, @dReq@ from
    -- @memUnitState@. A fetch goes out only when the FIFO has room for the
    -- pending write and this one both.
    coreOut =
      CoreOut
        { iReq = orNothing instResp.wreadyTwo MemBusReq {addr = ifPc, wdata = Nothing}
        , dReq = memUnitResp.memReq
        , instLog = maWbResp.rdata
        }

    state' =
      CoreState
        { ifPc = ifPc'
        , ifRequested = ifRequested'
        , ifFifoWdata = ifFifoWdata'
        , regFile = writeback regFile maWbResp.rdata
        , csrFile = csrFile'
        , memUnitState = memUnitState'
        }
