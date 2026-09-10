module Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo) where

import Clash.Prelude
import Data.Maybe (isJust, isNothing)

data FifoReq dat = FifoReq {wdata :: Maybe dat, rready :: Bool, flush :: Bool}
  deriving (Generic, NFDataX)

data FifoResp dat = FifoResp {wready :: Bool, rdata :: Maybe dat}
  deriving (Generic, NFDataX)

fifo :: forall dom dat. (HiddenClockResetEnable dom, NFDataX dat) => Signal dom (FifoReq dat) -> Signal dom (FifoResp dat)
fifo = mealy step Nothing
  where
    step buf FifoReq {..} = (buf', FifoResp {wready, rdata = buf})
      where
        wready = isNothing buf || rready
        buf'
          | flush = Nothing
          | wready, isJust wdata = wdata
          | rready = Nothing
          | otherwise = buf
