{- |
Core. 本の @core.veryl@ に対応する。

@core.veryl@ の @always_comb@ 一括記述に対応するのが 'coreT' で、1 サイクル分の
組合せロジックと次状態を 1 つの純粋関数として書き下している。'Signal' が現れるのは
'core' の配線層だけである。

本と意図的に変えた点:

* バスの有無を valid ビットではなく 'Maybe' で表す。
* FIFO を 'moore' で書き、出力が状態のみから決まることを型で保証している
  ('Cuintet.Fifo.fifoOutput')。これが core ↔ FIFO の組合せ循環を構造的に断ち切る。
* @memunit.veryl@ は module だが、ここでは 'Cuintet.MemUnit.memUnitStep' という
  純粋関数として呼び、状態は 'CoreState' が保持する。

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
import Data.Maybe (fromMaybe, isJust, isNothing)

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

-- | 1 命令分の実行記録。commit したサイクルにだけ出力される。
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

-- | ALU に渡す 2 つのオペランドを命令形式から選ぶ。
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

{- | Outputs the memory requests and the fetched instruction (if completed).

配線層。'mealy' を使うのは 'mealyB' が @Bundle@ インスタンスを要求するためで、
'mealy' なら制約なしで同じことができる。
-}
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

{- | 1 サイクル分の組合せロジックと次状態。本の @core.veryl@ の @always_comb@ に対応する。

@CoreIn@ を遅延パターンで受けるのは必須である。正格に受けると、@coreOut@ を WHNF まで
評価するだけで @CoreIn@ のコンストラクタが要求され、それは 'Cuintet.MemArbiter.memArbiter'
の正格な @MemArbiterReq@ パターンの評価に波及し、そこは @mkArbReq CoreOut{iReq, dReq}@
（@Cuintet.hs@）経由でまた 'coreT' に戻ってくる ── 評価が循環してハングする。
-}
coreT ::
  CoreState ->
  (CoreIn, FifoResp FifoEntry) ->
  (CoreState, (CoreOut, FifoReq FifoEntry))
coreT CoreState{..} (~CoreIn{iResp, dResp}, fifoResp) = (state', (coreOut, fifoReq))
 where
  -- FIFO 先頭の命令。空のサイクルでは X が入るが、@commit@ でゲートされるので届かない。
  instValid = isJust fifoResp.rdata
  FifoEntry{addr = pc, bits} = fromMaybe (deepErrorX "coreT: FIFO is empty") fifoResp.rdata

  -- データパス。有効性に関わらず無条件に計算する。
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

  -- FIFO の先頭を消費するのは、メモリアクセスが飛んでいない間だけ。
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

  -- 命令フェッチ。FIFO に保留中の書き込みと今回の分の両方の空きがあるときだけ出す。
  fetched = (,) <$> ifRequested <*> iResp.rdata
  busFree = isNothing ifRequested || isJust iResp.rdata -- not pending, or pending but fetched now
  accepted = fifoResp.wreadyTwo && iResp.ready && busFree -- fifo & memory & core available
  ifFifoWdata'
    | Just (a, b) <- fetched = Just FifoEntry{addr = a, bits = b}
    | fifoResp.wready = Nothing -- @ifFifoWdata@ was written at the previous clock.
    | otherwise = ifFifoWdata -- pending

  fifoReq = FifoReq{wdata = ifFifoWdata, rready}

  coreOut =
    CoreOut
      { iReq = orNothing fifoResp.wreadyTwo MemBusReq{addr = ifPc, wdata = Nothing}
      , dReq = memuOut.memreq
      , instLog =
          orNothing
            commit
            InstLog{pc, inst = bits, ctrl, imm, rs1Addr, rs2Addr, rs1Data, rs2Data, op1, op2, aluResult, wbReq}
      }

  state' =
    CoreState
      { ifPc = applyWhen accepted (+ 4) ifPc
      , ifRequested = if accepted then Just ifPc else ifRequested
      , ifFifoWdata = ifFifoWdata'
      , -- 消費したなら次に見えるのは新しい命令。先頭が空なら次は必ず新しい。
        isNew = if instValid then rready else True
      , regFile = maybe regFile (\(a, d) -> replace a d regFile) wbReq
      , memu = memu'
      }
