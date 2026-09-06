module Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring) where

import Clash.Prelude
import Cuintet.Upto (Upto (..), toMaybes)
import Data.Maybe (fromMaybe)

data RingReq nw nr dat = RingReq
  { wdata :: Upto nw dat
  , pop :: Index (nr + 1)
  , flush :: Bool
  }
  deriving (Generic, NFDataX)

data RingResp width nr dat = RingResp
  { rdata :: Upto nr dat
  , free :: Unsigned width
  }
  deriving (Generic, NFDataX)

data RingState width dat = RingState
  { hd :: Unsigned width
  , tl :: Unsigned width
  , buf :: Vec (2 ^ width) dat
  }
  deriving (Generic, NFDataX)

ring ::
  forall dom width nw nr dat.
  (HiddenClockResetEnable dom, KnownNat width, KnownNat nw, KnownNat nr, NFDataX dat, nw + 1 <= 2 ^ width, nr + 1 <= 2 ^ width) =>
  SNat width -> Signal dom (RingReq nw nr dat) -> Signal dom (RingResp width nr dat)
ring SNat = moore ringUpdate ringOutput initS
  where
    initS :: RingState width dat
    initS = RingState {hd = 0, tl = 0, buf = deepErrorX "ring: uninitialized"}

ringOutput ::
  forall width nr dat.
  (KnownNat width, KnownNat nr, nr <= 2 ^ width) =>
  RingState width dat -> RingResp width nr dat
ringOutput RingState {..} = RingResp {rdata = Upto {len, elems}, free}
  where
    used = tl - hd
    elems = (\i -> buf !! (hd + numConvert i)) <$> indicesI @nr
    len = fromMaybe maxBound (maybeNumConvert used)
    free = maxBound - used

ringUpdate ::
  forall width nw nr dat.
  (KnownNat width, KnownNat nw, KnownNat nr, NFDataX dat, nw + 1 <= 2 ^ width, nr + 1 <= 2 ^ width) =>
  RingState width dat -> RingReq nw nr dat -> RingState width dat
ringUpdate RingState {..} RingReq {..}
  | flush = RingState {hd = 0, tl = 0, buf = deepErrorX "ring: flushed"}
  | otherwise = RingState {hd = hd + numConvert pop, tl = tl + numConvert wdata.len, buf = buf'}
  where
    buf' = ifoldl write buf (toMaybes wdata)
    write b i = maybe b (\e -> replace (tl + numConvert i) e b)
