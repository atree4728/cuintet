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
module Cuintet.Core (FifoEntry (..), CoreIn (..), CoreOut (..), InstLog (..), core) where

import Clash.Prelude
import Cuintet.Alu (alu)
import Cuintet.BrUnit (brUnit)
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..), isBranchOp)
import Cuintet.CsrUnit (CsrAccess (..), CsrAddr (..), CsrFile, CsrReq (..), CsrResp (..), CsrTrap (..), csrUnitStep, initCsrFile, pattern ENVIRONMENT_CALL)
import Cuintet.Eei (
  Addr,
  Inst,
  MemBusReq (MemBusReq, addr, wdata),
  MemBusResp (rdata, ready),
  MemReq,
  MemResp,
  SystemOp (..),
  XLen,
  instAt,
 )
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.InstDecoder (instDecode)
import Cuintet.MemUnit (InstInfo (..), MemUnitReq (..), MemUnitResp (..), MemUnitState (..), memUnitStep)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

-- | Pair of fetched instruction and its address.
data FifoEntry = FifoEntry
  { addr :: Addr
  -- ^ The address of @bits@.
  , bits :: Inst
  -- ^ Fetched instruction.
  }
  deriving (Generic, NFDataX)

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
  , instLog :: Maybe InstLog
  }

-- | Execution log of a single instruction, emitted only in the clock it commits.
data InstLog = InstLog
  { pc :: Addr
  , inst :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: BitVector 5
  , rs2Addr :: BitVector 5
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Maybe Bool
  , wbReq :: Maybe (BitVector 5, BitVector XLen)
  , csrRdata :: Maybe (BitVector XLen)
  }
  deriving (Generic, NFDataX)

-- | Core state. The @if@ prefix is short for instruction fetch.
data CoreState = CoreState
  { ifPc :: Addr
  -- ^ Program counter.
  , ifRequested :: Maybe Addr
  -- ^ Address being fetched.
  , ifFifoWdata :: Maybe FifoEntry
  -- ^ The instruction waiting to be written.
  , isNew :: Bool
  -- ^ Whether the instruction at the FIFO's head is newly presented this cycle.
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
    , isNew = True
    , regFile = zeroBits :> replicate d31 (deepErrorX "register uninitialized")
    , csrFile = initCsrFile
    , memUnitState = Idle
    }

-- | Extract the two operands according to the instruction form.
operands ::
  InstCtrl ->
  BitVector XLen ->
  BitVector XLen ->
  BitVector XLen ->
  Addr ->
  (BitVector XLen, BitVector XLen)
operands InstCtrl {itype} imm rs1Data rs2Data pc = case itype of
  RType -> (rs1Data, rs2Data)
  BType -> (rs1Data, rs2Data)
  IType -> (rs1Data, imm)
  SType -> (rs1Data, imm)
  UType -> (bitCoerce pc, imm)
  JType -> (bitCoerce pc, imm)

-- | Closes 'coreT' around the instruction FIFO.
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core coreIn = coreOut
  where
    out = mealy coreT initState ((,) <$> coreIn <*> fifoResp)
    coreOut = fst <$> out
    fifoReq = snd <$> out
    fifoResp = fifo d3 fifoReq

-- | One clock of every stage, all combinational.
coreT ::
  CoreState ->
  (CoreIn, FifoResp FifoEntry) ->
  (CoreState, (CoreOut, FifoReq FifoEntry))
coreT CoreState {..} (~CoreIn {iResp, dResp}, fifoResp) = (state', (coreOut, fifoReq))
  where
    -- The head of the FIFO, @X@ while it is empty.
    instValid = isJust fifoResp.rdata
    FifoEntry {addr = pc, bits} = fromMaybe (deepErrorX "coreT: FIFO is empty") fifoResp.rdata

    -- ID and EX
    (ctrl, imm) = instDecode bits
    rs1Addr = slice d19 d15 bits
    rs2Addr = slice d24 d20 bits
    rs1Data = regFile !! rs1Addr
    rs2Data = regFile !! rs2Addr
    (op1, op2) = operands ctrl imm rs1Data rs2Data pc
    aluResult = alu ctrl op1 op2

    -- EX: the CSR access, and the redirect that @ECALL@ and @MRET@ answer with.
    -- A system instruction never stalls, so the request is raised in the clock
    -- it commits and @csrFile@ is never updated twice.
    (csrFile', csrResp) = maybe (csrFile, Nothing) (fmap Just . csrUnitStep csrFile) csrReq
    csrReq
      | not instValid = Nothing
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
          { inst = orNothing instValid InstInfo {isNew, ctrl, addr = bitCoerce aluResult, wdata = rs2Data}
          , memResp = dResp
          }

    -- WB: the instruction leaves once MA has let go of it, draining the FIFO.
    rready = not memUnitResp.stall
    commit = instValid && rready

    wbData
      | ctrl.isLui = imm
      | ctrl.isJump = bitCoerce (pc + 4)
      | ctrl.isLoad = fromMaybe (deepErrorX "coreT: load committed without data") memUnitResp.rdata
      | Just rdata <- csrRdata = rdata
      | otherwise = aluResult
    rdAddr = slice d11 d7 bits
    wbReq = orNothing (commit && ctrl.rwbEn && rdAddr /= 0) (rdAddr, wbData)

    -- Everything that redirects IF.
    branchTaken = brUnit ctrl.funct3 op1 op2
    controlHazard = instValid && (isJust csrRedirect || ctrl.isJump || isBranchOp ctrl && branchTaken)

    -- IF: the answer to the request issued when @ifRequested@ was set.
    fetched = (,) <$> ifRequested <*> iResp.rdata
    accepted = fifoResp.wreadyTwo && iResp.ready

    -- next state
    (ifPc', ifRequested')
      | controlHazard, Just redirect <- csrRedirect = (redirect, Nothing)
      | controlHazard && ctrl.isJump = (bitCoerce $ aluResult .&. complement 1, Nothing) -- aluResult is the destination
      | controlHazard && isBranchOp ctrl = (pc + numConvert imm, Nothing)
      | accepted = (ifPc + 4, Just ifPc)
      | otherwise = (ifPc, ifRequested)
    ifFifoWdata'
      | controlHazard = Nothing
      | Just (addr, busWord) <- fetched = Just FifoEntry {addr = addr, bits = instAt addr busWord}
      | fifoResp.wready = Nothing
      | otherwise = ifFifoWdata -- pending
    fifoReq = FifoReq {wdata = ifFifoWdata, rready, flush = controlHazard}

    -- Neither request may depend combinationally on @iResp@ or @dResp@, or a
    -- combinational loop closes through @memArbiter@. Both are driven straight
    -- from registers: @iReq@ from @ifPc@ and the FIFO, @dReq@ from
    -- @memUnitState@. A fetch goes out only when the FIFO has room for the
    -- pending write and this one both.
    coreOut =
      CoreOut
        { iReq = orNothing fifoResp.wreadyTwo MemBusReq {addr = ifPc, wdata = Nothing}
        , dReq = memUnitResp.memReq
        , instLog =
            orNothing
              commit
              InstLog
                { pc
                , inst = bits
                , ctrl
                , imm
                , rs1Addr
                , rs2Addr
                , rs1Data
                , rs2Data
                , op1
                , op2
                , aluResult
                , branchTaken = orNothing (instValid && isBranchOp ctrl) branchTaken
                , wbReq
                , csrRdata
                }
        }

    state' =
      CoreState
        { ifPc = ifPc'
        , ifRequested = ifRequested'
        , ifFifoWdata = ifFifoWdata'
        , isNew = not instValid || rready
        , regFile = maybe regFile (\(a, d) -> replace a d regFile) wbReq
        , csrFile = csrFile'
        , memUnitState = memUnitState'
        }
