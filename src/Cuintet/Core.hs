-- | Core.
module Cuintet.Core where

import Clash.Prelude
import Cuintet.Corectrl (InstCtrl)
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
  }
  deriving (Generic, NFDataX)

-- | Outputs the memory request and the fetched instruction (if completed).
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom (MemBusResp ILen) ->
  Signal dom (MemBusReq ILen XLen, Maybe (Addr, InstCtrl, BitVector XLen))
core memResp = bundle (memReq, instInfo)
 where
  coreState =
    register
      CoreState
        { ifPc = 0
        , ifRequested = Nothing
        , ifFifoWdata = Nothing
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

  fifoEntry = (.rdata) <$> fifoResp
  instPc = (.addr) <$$> fifoEntry
  instBits = (.bits) <$$> fifoEntry
  decoded = instDecode <$$> instBits
  instInfo = liftAA2 (\addr (ctrl, imm) -> (addr, ctrl, imm)) instPc decoded

  -- TODO: non-redundant state machine
  -- TODO: @deepErrorX@ for unexpected situation
  coreT CoreState{..} MemBusResp{..} FifoResp{wready, wreadyTwo} =
    CoreState
      { ifPc = applyWhen accepted (+ 4) ifPc
      , ifRequested = ifRequested'
      , ifFifoWdata = ifFifoWdata'
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
