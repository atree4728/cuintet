-- | FIFO that stores up to 2^@width@ - 1 elements of type @dat@.
module Cuintet.Unit.Fifo (FifoReq (..), FifoResp (..), fifo) where

import Clash.Prelude
import Cuintet.Util (orNothing)
import Data.Function (applyWhen)
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
  , wreadyTwo :: Bool
  -- ^ Whether the FIFO can accept two consecutive writes.
  , rdata :: Maybe dat
  -- ^ The oldest element.
  }
  deriving (Generic, NFDataX, Show)

-- | FIFO with a capacity of 2^@width@ - 1 elements.
fifo ::
  forall dom width dat.
  (HiddenClockResetEnable dom, KnownNat width, NFDataX dat) =>
  SNat width ->
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifo width = case compareSNat width d1 of
  SNatLE -> fifoOne
  SNatGT -> fifoMany width

-- | Single-entry FIFO, the @width == 1@ case of 'fifo'. Unlike 'fifoMany', it accepts a write even when full if the stored element is consumed in the same cycle.
fifoOne ::
  forall dom dat.
  (HiddenClockResetEnable dom, NFDataX dat) =>
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifoOne = mealy step Nothing
  where
    step buf FifoReq {..}
      | flush = (Nothing, FifoResp {wready = False, wreadyTwo = False, rdata = Nothing})
      | otherwise = (buf', FifoResp {wready, wreadyTwo, rdata})
      where
        -- accept write when the buffer is either already empty or going to be emptied by read.
        wready = isNothing buf || rready
        wreadyTwo = False
        rdata = buf
        buf'
          | wready && isJust wdata = wdata
          | rready = Nothing
          | otherwise = buf

-- | The state of @fifoMany@.
data FifoState width dat = FifoState
  { hd :: Unsigned width
  , tl :: Unsigned width
  , buf :: Vec (2 ^ width) dat
  }
  deriving (Generic, NFDataX)

fifoOutput :: (KnownNat width) => FifoState width dat -> FifoResp dat
fifoOutput FifoState {hd, tl, buf} = FifoResp {wready, wreadyTwo, rdata}
  where
    wready = tl + 1 /= hd
    wreadyTwo = wready && tl + 2 /= hd
    rdata = orNothing (hd /= tl) (buf !! hd)

fifoUpdate :: (KnownNat width, NFDataX dat) => FifoState width dat -> FifoReq dat -> FifoState width dat
fifoUpdate s@FifoState {hd, tl, buf} FifoReq {wdata, rready, flush}
  | flush = FifoState {hd = 0, tl = 0, buf = deepErrorX "fifoMany: flushed"}
  | otherwise = FifoState {hd = hd', tl = tl', buf = buf'}
  where
    FifoResp {wready, rdata} = fifoOutput s
    hd' = applyWhen (rready && isJust rdata) (+ 1) hd
    (tl', buf')
      | wready, Just entry <- wdata = (tl + 1, replace tl entry buf)
      | otherwise = (tl, buf)

{- | Ring-buffer FIFO, the @width >= 2@ case of 'fifo'.

One of the 2^@width@ slots is kept unused to distinguish full from empty.
@rdata@ is at least one-cycle delayed.
-}
fifoMany ::
  forall dom width dat.
  (HiddenClockResetEnable dom, KnownNat width, NFDataX dat) =>
  SNat width ->
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifoMany SNat = moore fifoUpdate fifoOutput initS
  where
    initS :: FifoState width dat
    initS = FifoState {hd = 0, tl = 0, buf = deepErrorX "fifoMany: uninitialized"}
