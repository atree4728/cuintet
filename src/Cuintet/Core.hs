module Cuintet.Core where

import Clash.Prelude
import Cuintet.Eei
import Cuintet.Fifo (FifoReq (..), FifoResp (..), fifo)
import Data.Maybe (isJust, isNothing)

data FifoEntry = FifoEntry
  { addr :: Addr
  -- ^ The address of @bits@.
  , bits :: Inst
  -- ^ Fetched instruction.
  }
  deriving (Generic, NFDataX, Eq, Show)

-- | "if" is a shorthand of instruction fetch.
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
  Signal dom (MemBusReq ILen XLen, Maybe FifoEntry)
core memResp = bundle (memReq, inst)
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
      { wvalid = isJust ifFifoWdata
      , wdata = fromJustX ifFifoWdata
      , rready = True
      }
  fifoResp = fifo d3 fifoReq

  -- Fetch only when the FIFO has room for both the pending write and this fetch.
  memReq = mkMemReq <$> coreState <*> fifoResp
  mkMemReq CoreState{ifPc} FifoResp{wready_two} =
    MemBusReq
      { valid = wready_two
      , addr = ifPc
      , wdata = Nothing
      }

  inst = (\FifoResp{rvalid, rdata} -> if rvalid then Just rdata else Nothing) <$> fifoResp

  coreT s@CoreState{..} MemBusResp{..} FifoResp{wready, wready_two} =
    s
      { ifPc = if accepted then ifPc + 4 else ifPc
      , ifRequested = ifRequested'
      , ifFifoWdata = ifFifoWdata'
      }
   where
    fetched = (,) <$> ifRequested <*> rdata
    busFree = isNothing ifRequested || isJust rdata
    accepted = wready_two && ready && busFree
    ifRequested'
      | accepted = Just ifPc
      | otherwise = ifRequested
    ifFifoWdata'
      | Just (addr, bits) <- fetched = Just FifoEntry{addr, bits}
      | wready = Nothing
      | otherwise = ifFifoWdata
