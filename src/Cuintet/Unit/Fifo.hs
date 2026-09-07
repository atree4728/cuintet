module Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo) where

import Clash.Prelude
import Cuintet.Upto (Upto)
import Cuintet.Upto qualified as Upto

-- | FIFO Request.
data FifoReq n dat = FifoReq
  { wdata :: Upto n dat
  -- ^ Data to write.
  , rready :: Bool
  -- ^ Whether to consume @rdata@.
  , flush :: Bool
  -- ^ Whether to flush the FIFO.
  }
  deriving (Generic, NFDataX)

-- | FIFO Response.
data FifoResp n dat = FifoResp
  { wready :: Bool
  -- ^ Whether the FIFO can accept a write.
  , rdata :: Upto n dat
  -- ^ The oldest element.
  }
  deriving (Generic, NFDataX)

fifo ::
  forall dom n dat.
  (HiddenClockResetEnable dom, KnownNat n, NFDataX dat) =>
  Signal dom (FifoReq n dat) ->
  Signal dom (FifoResp n dat)
fifo = mealy step Upto.empty
  where
    step buf FifoReq {..} = (buf', FifoResp {wready, rdata = buf})
      where
        -- accept write when the buffer is either already empty or going to be emptied by read.
        wready = buf.len == 0 || rready
        buf'
          | flush = Upto.empty
          | wready && wdata.len > 0 = wdata
          | rready = Upto.empty
          | otherwise = buf
