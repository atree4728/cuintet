-- | FIFO that stores up to 2^@width@ - 1 elements of type @dat@.
module Cuintet.Fifo where

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

{- | FIFO with a capacity of 2^@width@ - 1 elements.

A write is accepted when @wready && wvalid@, and the oldest element is
consumed when @rvalid && rready@, both in the same cycle if needed.
Supplied @wdata@ when not writable is ignored, but this is not expected situation.
-}
fifo ::
  forall dom width dat.
  (HiddenClockResetEnable dom, KnownNat width, 1 <= width, NFDataX dat) =>
  SNat width ->
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifo width = case compareSNat width d1 of
  SNatLE -> fifoOne
  SNatGT -> fifoMany width

{- | Single-entry FIFO, the @width == 1@ case of 'fifo'.

Unlike 'fifoMany', it accepts a write even when full if the stored element is consumed in the same cycle.

>>> import Prelude
>>> import Clash.Prelude
>>> reqs = (\(wdata, rready) -> FifoReq {wdata, rready}) <$> [(Just 1, True), (Just 2, False), (Just 3, True), (Just 4, False), (Just 5, False), (Just 6, True), (Just 7, True)]
>>> (.rdata) <$> simulateN @System (Prelude.length reqs) (fifo d1) reqs
[Nothing,Just 1,Just 1,Just 3,Just 3,Just 3,Just 6]
-}
fifoOne ::
  forall dom dat.
  (HiddenClockResetEnable dom, NFDataX dat) =>
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifoOne = mealy step Nothing
 where
  step buf FifoReq{..} = (buf', FifoResp{wready, wreadyTwo, rdata})
   where
    -- accept write when the buffer is either already empty or going to be emptied by read.
    wready = isNothing buf || rready
    wreadyTwo = False
    rdata = buf
    buf'
      | wready && isJust wdata = wdata
      | rready = Nothing
      | otherwise = buf

{- | Ring-buffer FIFO, the @width >= 2@ case of 'fifo'.

One of the 2^@width@ slots is kept unused to distinguish full from empty.
@rdata@ is at least one-cycle delayed.

>>> import Prelude
>>> import Clash.Prelude
>>> reqs = (\(wdata, rready) -> FifoReq {wdata, rready}) <$> [(Just 1, True), (Just 2, False), (Just 3, True), (Just 4, False), (Just 5, False), (Just 6, True), (Just 7, True)]
>>> (.rdata) <$> simulateN @System (Prelude.length reqs) (fifo d3) reqs
[Nothing,Just 1,Just 1,Just 2,Just 2,Just 2,Just 3]
-}
fifoMany ::
  forall dom width dat.
  (HiddenClockResetEnable dom, KnownNat width, 2 <= width, NFDataX dat) =>
  SNat width ->
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifoMany SNat = mealy step initS
 where
  initS :: (Unsigned width, Unsigned width, Vec (2 ^ width) dat)
  initS = (0, 0, deepErrorX "fifoMany: uninitialized")

  step (hd, tl, buf) FifoReq{..} = ((hd', tl', buf'), FifoResp{..})
   where
    wready = tl + 1 /= hd
    wreadyTwo = wready && tl + 2 /= hd
    rvalid = hd /= tl
    rdata = orNothing rvalid (buf !! hd)
    hd' = applyWhen (rready && rvalid) (+ 1) hd
    (tl', buf')
      | wready, Just entry <- wdata = (tl + 1, replace tl entry buf)
      | otherwise = (tl, buf)
