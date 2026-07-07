-- | Core.
module Cuintet.Core where

import Clash.Prelude
import Cuintet.Debug (InstLog)
import Cuintet.Eei
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Cuintet.InstDecoder (instDecode)
import Cuintet.Util
import Data.Function (applyWhen)
import Data.Maybe (isJust, isNothing)

-- | Pair of fetched instruction and its address.
data FifoEntry = FifoEntry
  { addr :: Addr
  -- ^ The address of @bits@.
  , bits :: Inst
  -- ^ Fetched instruction.
  }
  deriving (Generic, NFDataX)

-- | core state. "if" is a shorthand of instruction fetch.
data CoreState
  = CoreState
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

-- | Outputs the memory request and the fetched instruction (if completed).
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom (MemBusResp ILen) ->
  Signal dom (MemBusReq ILen XLen, Maybe InstLog)
core memResp = bundle (memReq, instLog)
 where
  coreState =
    register
      CoreState
        { ifPc = 0
        , ifRequested = Nothing
        , ifFifoWdata = Nothing
        , regFile = generate d32 (+ 1) (-1 :: BitVector XLen)
        }
      (coreT <$> coreState <*> memResp <*> fifoResp)

  fifoReq = mkFifoReq <$> coreState
  mkFifoReq CoreState{ifFifoWdata} =
    FifoReq
      { wdata = ifFifoWdata
      , rready = True
      }
  fifoResp = fifo d3 fifoReq

  -- Fetch only when the FIFO has room for both the pending write and this fetch.
  memReq = mkMemReq <$> coreState <*> fifoResp
  mkMemReq CoreState{ifPc} FifoResp{wreadyTwo} =
    MemBusReq
      { valid = wreadyTwo
      , addr = ifPc
      , wdata = Nothing
      }

  -- decode fetched instruction
  fifoEntry = (.rdata) <$> fifoResp
  instPc = (.addr) <<$>> fifoEntry
  instBits = (.bits) <<$>> fifoEntry
  decoded = instDecode <<$>> instBits

  rs1Addr = slice d19 d15 <<$>> instBits
  rs2Addr = slice d24 d20 <<$>> instBits

  reg = (.regFile) <$> coreState

  rs1Data = (!!) <<$>> (pure <$> reg) <<*>> rs1Addr
  rs2Data = (!!) <<$>> (pure <$> reg) <<*>> rs2Addr

  instLog = (,,,,,,) <<$>> instPc <<*>> instBits <<*>> decoded <<*>> rs1Addr <<*>> rs2Addr <<*>> rs1Data <<*>> rs2Data

  -- TODO: non-redundant state machine
  -- TODO: @deepErrorX@ for unexpected situation
  coreT CoreState{..} MemBusResp{..} FifoResp{wready, wreadyTwo} =
    CoreState
      { ifPc = applyWhen accepted (+ 4) ifPc
      , ifRequested = ifRequested'
      , ifFifoWdata = ifFifoWdata'
      , regFile
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
