-- | Core.
module Cuintet.Core where

import Clash.Prelude
import Control.Monad (join)
import Cuintet.Alu (alu)
import Cuintet.Corectrl (InstCtrl (..), InstType (..))
import Cuintet.Eei
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.InstDecoder (instDecode)
import Cuintet.MemUnit (InstInfo (..), MemUnitReq (..), MemUnitResp (..), memUnit)
import Cuintet.Util
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

-- | core state. "if" is a shorthand of instruction fetch.
data State = State
  { ifPc :: Addr
  -- ^ Program counter.
  , ifRequested :: Maybe Addr
  -- ^ Address being fetched.
  , ifFifoWdata :: Maybe FifoEntry
  -- ^ The instruction waiting to be written.
  , regFile :: Vec 32 (BitVector XLen)
  -- ^ Register file.
  }
  deriving (Generic, NFDataX)

type InstLog =
  ( Addr
  , Inst
  , ( InstCtrl
    , BitVector XLen
    )
  , BitVector 5
  , BitVector 5
  , BitVector XLen
  , BitVector XLen
  , (BitVector XLen, BitVector XLen)
  , BitVector XLen
  , Maybe (BitVector 5, BitVector XLen)
  )

-- | Outputs the memory requests and the fetched instruction (if completed).
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core coreIn = CoreOut <$> iReqOut <*> dReqOut <*> instLog
 where
  iResp' = (.iResp) <$> coreIn
  dResp' = (.dResp) <$> coreIn

  state =
    register
      State
        { ifPc = 0
        , ifRequested = Nothing
        , ifFifoWdata = Nothing
        , regFile = replicate d32 0
        }
      (coreT <$> state <*> iResp' <*> fifoResp <*> commit <*> wbReq')

  fifoReq = mkFifoReq <$> state <*> rready
  mkFifoReq State{ifFifoWdata} rdy =
    FifoReq
      { wdata = ifFifoWdata
      , rready = rdy
      }
  fifoResp = fifo d3 fifoReq

  -- Fetch only when the FIFO has room for both the pending write and this fetch.
  iReqOut = mkIReq <$> state <*> fifoResp
  mkIReq State{ifPc} FifoResp{wreadyTwo} =
    orNothing wreadyTwo MemBusReq{addr = ifPc, wdata = Nothing}

  fifoEntry = (.rdata) <$> fifoResp
  instPc = (.addr) <<$>> fifoEntry
  instBits = (.bits) <<$>> fifoEntry
  decoded = instDecode <<$>> instBits

  rs1Addr = slice d19 d15 <<$>> instBits
  rs2Addr = slice d24 d20 <<$>> instBits

  reg = (.regFile) <$> state

  rs1Data = (!!) <<$>> (pure <$> reg) <<*>> rs1Addr
  rs2Data = (!!) <<$>> (pure <$> reg) <<*>> rs2Addr

  ops = mkOps <<$>> decoded <<*>> rs1Data <<*>> rs2Data <<*>> instPc
  mkOps :: (InstCtrl, BitVector XLen) -> BitVector XLen -> BitVector XLen -> Addr -> (BitVector XLen, BitVector XLen)
  mkOps (InstCtrl{itype}, imm) rs1d rs2d pc =
    case itype of
      RType -> (rs1d, rs2d)
      BType -> (rs1d, rs2d)
      IType -> (rs1d, imm)
      SType -> (rs1d, imm)
      UType -> (bitCoerce pc, imm)
      JType -> (bitCoerce pc, imm)

  aluResult = (\(ctrl, _) (op1, op2) -> alu ctrl op1 op2) <<$>> decoded <<*>> ops

  -- Whether the instruction at the FIFO's head is newly presented this cycle,
  -- i.e. was not already seen (and possibly stalled on) last cycle.
  isNew = register True nextIsNew
  nextIsNew = mkNextIsNew <$> fifoEntry <*> rready
  mkNextIsNew entry rr = maybe True (const rr) entry

  instInfo = mkInstInfo <$> isNew <*> decoded <*> aluResult <*> rs2Data
  mkInstInfo isNew' mDec mAluRes mRs2 = do
    (ctrl, _) <- mDec
    aluRes <- mAluRes
    rs2 <- mRs2
    pure InstInfo{isNew = isNew', ctrl, addr = bitCoerce aluRes, rs2}

  memuReq = mkMemuReq <$> instInfo <*> dResp'
  mkMemuReq inst memresp = MemUnitReq{inst, memresp}
  memuResp = memUnit memuReq
  memuRdata = (.rdata) <$> memuResp
  dReqOut = (.memreq) <$> memuResp

  -- Stall the FIFO (stop consuming) while the memory unit has an access in flight.
  stall = (.stall) <$> memuResp
  rready = not <$> stall

  -- The FIFO's head is committed (consumed, and its effects take place) exactly
  -- when it is present and consumed this cycle.
  commit = mkCommit <$> fifoEntry <*> rready
  mkCommit entry rr = isJust entry && rr

  wbReqPre = join <$> (mkWbReqPre <<$>> decoded <<*>> instBits <<*>> aluResult)
  mkWbReqPre :: (InstCtrl, BitVector XLen) -> Inst -> BitVector XLen -> Maybe (BitVector 5, Bool, BitVector XLen)
  mkWbReqPre (InstCtrl{isLui, isLoad, rwbEn}, imm) bits aluRes = orNothing rwbEn (rdAddr, isLoad, wbData)
   where
    rdAddr = slice d11 d7 bits
    wbData
      | isLui = imm
      | otherwise = aluRes

  -- Loads override the ALU-computed value with the data read from memory,
  -- which only becomes available on the commit cycle.
  wbReq' = combineWb <$> wbReqPre <*> memuRdata
  combineWb mPre mMemuRd = do
    (rdAddr, isLoad, wbDataAlu) <- mPre
    pure (rdAddr, if isLoad then fromMaybe wbDataAlu mMemuRd else wbDataAlu)

  logAll = addWbReq <$> presence <*> wbReq'
  presence = (,,,,,,,,) <<$>> instPc <<*>> instBits <<*>> decoded <<*>> rs1Addr <<*>> rs2Addr <<*>> rs1Data <<*>> rs2Data <<*>> ops <<*>> aluResult
  addWbReq mTuple wbReq =
    (\(pc, bits, dec, rs1a, rs2a, rs1d, rs2d, o, aluRes) -> (pc, bits, dec, rs1a, rs2a, rs1d, rs2d, o, aluRes, wbReq)) <$> mTuple

  -- Only report an instruction once, on the cycle it commits.
  instLog = gateLog <$> commit <*> logAll
  gateLog c l = if c then l else Nothing

  -- TODO: non-redundant state machine
  -- TODO: @deepErrorX@ for unexpected situation
  coreT State{..} MemBusResp{..} FifoResp{wready, wreadyTwo} isCommit wbReq =
    State
      { ifPc = applyWhen accepted (+ 4) ifPc
      , ifRequested = ifRequested'
      , ifFifoWdata = ifFifoWdata'
      , regFile = regFile'
      }
   where
    fetched = (,) <$> ifRequested <*> rdata
    busFree = isNothing ifRequested || isJust rdata -- not pending, or pending but fetched now
    accepted = wreadyTwo && ready && busFree -- fifo & memory & core available
    ifRequested'
      | accepted = Just ifPc
      | otherwise = ifRequested
    ifFifoWdata'
      | Just (addr, bits) <- fetched = Just FifoEntry{addr, bits} -- @ifFifoWdata@ is to be Nothing, because instruction fetch is enabled only when @wreadyTwo@.
      | wready = Nothing -- @ifFifoWdata@ was writtern at the previous clock.
      | otherwise = ifFifoWdata -- pending
    regFile'
      | isCommit = maybe regFile (\(addr, dat) -> replace addr dat regFile) wbReq
      | otherwise = regFile
