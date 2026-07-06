-- | FIFO that stores up to 2^@width@ - 1 elements of type @dataT@.
module Cuintet.Fifo where

import Clash.Prelude
import Data.Function (applyWhen)

data FifoReq dat = FifoReq
  { wvalid :: Bool
  -- ^ Whether to write @wdata@.
  , wdata :: dat
  -- ^ Data to write.
  , rready :: Bool
  -- ^ Whether to consume @rdata@.
  }
  deriving (Generic, NFDataX)

data FifoResp dat = FifoResp
  { wready :: Bool
  -- ^ Whether the FIFO can accept a write.
  , wready_two :: Bool
  -- ^ Whether the FIFO can accept two consecutive writes.
  , rvalid :: Bool
  -- ^ Whether @rdata@ is valid.
  , rdata :: dat
  -- ^ The oldest element.
  }
  deriving (Generic, NFDataX)

{- | FIFO with a capacity of 2^@width@ - 1 elements.

A write is accepted when @wready && wvalid@, and the oldest element is
consumed when @rvalid && rready@, both in the same cycle if needed.
-}
fifo ::
  forall dom width dat.
  (HiddenClockResetEnable dom, KnownNat width, NFDataX dat) =>
  SNat width ->
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifo width = case compareSNat width d1 of
  SNatLE -> fifoOne
  SNatGT -> fifoMany width

{- | Single-entry FIFO, the @width == 1@ case of 'fifo'.

Unlike 'fifoMany', it accepts a write even when full if the stored
element is consumed in the same cycle.
-}
fifoOne ::
  forall dom dat.
  (HiddenClockResetEnable dom, NFDataX dat) =>
  Signal dom (FifoReq dat) ->
  Signal dom (FifoResp dat)
fifoOne = mealy step initS
 where
  initS = (False, deepErrorX "fifoOne: empty")
  step (prevRvalid, prevRdata) FifoReq{..} = ((rvalid, rdata), resp)
   where
    resp = FifoResp{wready, wready_two, rvalid, rdata}
    wready = not prevRvalid || rready
    wready_two = False
    (rvalid, rdata)
      | wready && wvalid = (True, wdata)
      | rready = (False, deepErrorX "fifoOne: empty")
      | otherwise = (prevRvalid, prevRdata)

{- | Ring-buffer FIFO, the @width >= 2@ case of 'fifo'.

One of the 2^@width@ slots is kept unused to distinguish full from empty.
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
  initS = (0, 0, deepErrorX "fifoMany: empty")

  step (hd, tl, mem) FifoReq{..} = ((hd', tl', mem'), FifoResp{..})
   where
    wready = tl + 1 /= hd
    wready_two = wready && tl + 2 /= hd
    rvalid = hd /= tl
    rdata = mem !! hd
    hd' = applyWhen (rready && rvalid) (+ 1) hd
    tl' = applyWhen (wready && wvalid) (+ 1) tl
    mem' = applyWhen (wready && wvalid) (replace tl wdata) mem
