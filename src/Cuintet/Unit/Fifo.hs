module Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo) where

import Clash.Prelude
import Data.Maybe (isJust, isNothing)

-- | FIFO Request.
data FifoReq dat = FifoReq
  { wdata :: Maybe dat
  -- ^ Data to write.
  , rready :: Bool
  -- ^ Whether to consume @rdata@.
  , flush :: Bool
  -- ^ Whether to flush the FIFO.
  }
  deriving (Generic, NFDataX, Show)

-- | FIFO Response.
data FifoResp dat = FifoResp
  { wready :: Bool
  -- ^ Whether the FIFO can accept a write.
  , rdata :: Maybe dat
  -- ^ The oldest element.
  }
  deriving (Generic, NFDataX, Show)

fifo ::
  forall dom dat.
  (HiddenClockResetEnable dom, NFDataX dat) =>
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifo = mealy step Nothing
  where
    step buf FifoReq {..} = (buf', FifoResp {wready, rdata})
      where
        -- accept write when the buffer is either already empty or going to be emptied by read.
        wready = isNothing buf || rready
        rdata = buf
        buf'
          | flush = Nothing
          | wready && isJust wdata = wdata
          | rready = Nothing
          | otherwise = buf
