module Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring) where

import Clash.Prelude
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe)

data RingReq nw nr dat = RingReq
  { wdata :: Upto nw dat
  , pop :: Index (nr + 1)
  , flush :: Bool
  }
  deriving (Generic, NFDataX)

data RingResp bits nr dat = RingResp
  { rdata :: Upto nr dat
  , free :: Unsigned bits
  }
  deriving (Generic, NFDataX)

data RingState bits dat = RingState
  { hd :: Unsigned bits
  , tl :: Unsigned bits
  , buf :: Vec (2 ^ bits) dat
  }
  deriving (Generic, NFDataX)

ring ::
  forall dom bits nw nr dat.
  (HiddenClockResetEnable dom, KnownNat bits, KnownNat nw, KnownNat nr, NFDataX dat, nw + 1 <= 2 ^ bits, nr + 1 <= 2 ^ bits) =>
  SNat bits -> Signal dom (RingReq nw nr dat) -> Signal dom (RingResp bits nr dat)
ring SNat = moore ringUpdate ringOutput initS
  where
    initS :: RingState bits dat
    initS = RingState {hd = 0, tl = 0, buf = deepErrorX "ring: uninitialized"}

ringOutput ::
  forall bits nr dat.
  (KnownNat bits, KnownNat nr, nr <= 2 ^ bits) =>
  RingState bits dat -> RingResp bits nr dat
ringOutput RingState {..} = RingResp {rdata = Upto {len, elems}, free}
  where
    used = tl - hd
    elems = (\i -> buf !! (hd + numConvert i)) <$> indicesI @nr
    len = fromMaybe maxBound (maybeNumConvert used)
    free = maxBound - used

ringUpdate ::
  forall bits nw nr dat.
  (KnownNat bits, KnownNat nw, KnownNat nr, NFDataX dat, nw + 1 <= 2 ^ bits, nr + 1 <= 2 ^ bits) =>
  RingState bits dat -> RingReq nw nr dat -> RingState bits dat
ringUpdate RingState {..} RingReq {..}
  | flush = RingState {hd = 0, tl = 0, buf = deepErrorX "ring: flushed"}
  | otherwise = RingState {hd = hd + numConvert pop, tl = tl + numConvert wdata.len, buf = buf'}
  where
    buf' = ifoldl write buf (Upto.toMaybes wdata)
    write b i = maybe b (\e -> replace (tl + numConvert i) e b)
