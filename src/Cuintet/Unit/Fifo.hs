module Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo) where

import Clash.Prelude
import Data.Maybe (isNothing)

data FifoReq n dat = FifoReq {wdata :: Vec n (Maybe dat), rready :: Bool, flush :: Bool}
  deriving (Generic, NFDataX)

data FifoResp n dat = FifoResp {wready :: Bool, rdata :: Vec n (Maybe dat)}
  deriving (Generic, NFDataX)

fifo :: forall dom n dat. (HiddenClockResetEnable dom, KnownNat n, NFDataX dat) => Signal dom (FifoReq n dat) -> Signal dom (FifoResp n dat)
fifo = mealy step (repeat Nothing)
  where
    step buf FifoReq {..} = (buf', FifoResp {wready, rdata = buf})
      where
        wready = all isNothing buf || rready
        buf'
          | flush = repeat Nothing
          | wready = wdata
          | otherwise = buf
