{- |
本と意図的に変えた点:

* バスの有無を valid ビットではなく 'Maybe' で表す。
* FIFO を 'moore' で書き、出力が状態のみから決まることを型で保証している
  ('Cuintet.Fifo.fifoOutput')。これが core ↔ FIFO の組合せ循環を構造的に断ち切る。
* @memunit.veryl@ は module だが、ここでは 'Cuintet.MemUnit.memUnitStep' という
  純粋関数として呼び、状態は t'CoreState' が保持する。

'coreT' が守るべき不変条件: @iReq@ と @dReq@ は @iResp@ / @dResp@ に組合せ依存して
はならない。依存させると @memArbiter@ との間に組合せループができる。
-}
module Cuintet.Core where

import Clash.Prelude
import Cuintet.Alu (alu)
import Cuintet.Corectrl (InstCtrl (..), InstType (..))
import Cuintet.Eei
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.InstDecoder (instDecode)
import Cuintet.MemUnit (InstInfo (..), MemUnitReq (..), MemUnitResp (..), MemUnitState (..), memUnitStep)
import Cuintet.Util (orNothing)
import Data.Function (applyWhen)
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
  { iResp :: InstResp
  -- ^ Response to the instruction fetch request.
  , dResp :: DataResp
  -- ^ Response to the load/store request.
  }

data CoreOut = CoreOut
  { iReq :: Maybe InstReq
  -- ^ Instruction fetch request.
  , dReq :: Maybe DataReq
  -- ^ Load/store request.
  , instLog :: Maybe InstLog
  }

-- | An execution log for a single instruction, which is output only in the cycle in which the commit occurred.
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
  , wbReq :: Maybe (BitVector 5, BitVector XLen)
  }
  deriving (Generic, NFDataX)

-- | core state. "if" is a shorthand of instruction fetch.
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
  , memu :: MemUnitState
  -- ^ Memory unit's FSM.
  }
  deriving (Generic, NFDataX)

initState :: CoreState
initState =
  CoreState
    { ifPc = 0
    , ifRequested = Nothing
    , ifFifoWdata = Nothing
    , isNew = True
    , regFile = replicate d32 0
    , memu = Init
    }

-- | Extract the two operands according to the instruction form.
operands ::
  InstCtrl ->
  BitVector XLen ->
  BitVector XLen ->
  BitVector XLen ->
  Addr ->
  (BitVector XLen, BitVector XLen)
operands InstCtrl{itype} imm rs1Data rs2Data pc = case itype of
  RType -> (rs1Data, rs2Data)
  BType -> (rs1Data, rs2Data)
  IType -> (rs1Data, imm)
  SType -> (rs1Data, imm)
  UType -> (bitCoerce pc, imm)
  JType -> (bitCoerce pc, imm)

-- | Outputs the memory requests and the fetched instruction (if completed).
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

-- | Combinatrial logic of the core.
coreT ::
  CoreState ->
  (CoreIn, FifoResp FifoEntry) ->
  (CoreState, (CoreOut, FifoReq FifoEntry))
coreT CoreState{..} (~CoreIn{iResp, dResp}, fifoResp) = (state', (coreOut, fifoReq))
 where
  -- The top of the instruction FIFO, possibly unstable @X@
  instValid = isJust fifoResp.rdata
  FifoEntry{addr = pc, bits} = fromMaybe (deepErrorX "coreT: FIFO is empty") fifoResp.rdata

  -- data path
  (ctrl, imm) = instDecode bits
  rs1Addr = slice d19 d15 bits
  rs2Addr = slice d24 d20 bits
  rs1Data = regFile !! rs1Addr
  rs2Data = regFile !! rs2Addr
  (op1, op2) = operands ctrl imm rs1Data rs2Data pc
  aluResult = alu ctrl op1 op2

  (memu', memuOut) =
    memUnitStep
      memu
      MemUnitReq
        { inst = orNothing instValid InstInfo{isNew, ctrl, addr = bitCoerce aluResult, rs2 = rs2Data}
        , memresp = dResp
        }

  -- Consumes the FIFO top when not accessing memory
  rready = not memuOut.stall
  commit = instValid && rready

  -- 不変条件 memu == 'Init' → isNew == True （core と memUnit にまたがる）により、
  -- load が commit するとき @memuOut.rdata@ は必ず 'Just'。
  wbData
    | ctrl.isLui = imm
    | ctrl.isLoad = fromMaybe (deepErrorX "coreT: load committed without data") memuOut.rdata
    | otherwise = aluResult
  rdAddr = slice d11 d7 bits
  wbReq = orNothing (commit && ctrl.rwbEn && rdAddr /= 0) (rdAddr, wbData)

  -- Instruction fetch.
  -- FIFO に保留中の書き込みと今回の分の両方の空きがあるときだけ出す。
  fetched = (,) <$> ifRequested <*> iResp.rdata
  -- @iReq@ が出ていて（@wreadyTwo@）、かつ @dReq@ に阻まれていない（@ready@）なら
  -- メモリが受理した。arbiter は受理に必ず応答を返すので、これで @ifPc@ と
  -- @ifRequested@ が同期する。
  accepted = fifoResp.wreadyTwo && iResp.ready
  ifFifoWdata'
    | Just (a, b) <- fetched = Just FifoEntry{addr = a, bits = b}
    -- 前クロックで書き込み済み。フェッチは @wreadyTwo@ のときしか出さないので、
    -- 保留が残っていることはなく Nothing でよい。
    | fifoResp.wready = Nothing
    | otherwise = ifFifoWdata -- pending
  fifoReq = FifoReq{wdata = ifFifoWdata, rready}

  coreOut =
    CoreOut
      { iReq = orNothing fifoResp.wreadyTwo MemBusReq{addr = ifPc, wdata = Nothing}
      , dReq = memuOut.memreq
      , instLog = orNothing commit InstLog{pc, inst = bits, ctrl, imm, rs1Addr, rs2Addr, rs1Data, rs2Data, op1, op2, aluResult, wbReq}
      }

  state' =
    CoreState
      { ifPc = applyWhen accepted (+ 4) ifPc
      , ifRequested = if accepted then Just ifPc else ifRequested
      , ifFifoWdata = ifFifoWdata'
      , isNew = not instValid || rready
      , regFile = maybe regFile (\(a, d) -> replace a d regFile) wbReq
      , memu = memu'
      }
