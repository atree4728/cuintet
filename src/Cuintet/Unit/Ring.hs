-- | A queue entered and left in order.
module Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring) where

import Clash.Prelude
import Cuintet.Unit.MultiRam (multiRam)
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

data RingReq bits nw nr dat = RingReq
  { wdata :: Vec nw (Maybe dat)
  , pop :: Index (nr + 1)
  , squash :: Bool
  }
  deriving (Generic, NFDataX)

data RingResp bits nr dat = RingResp
  { rdata :: Vec nr (Maybe dat)
  , free :: Unsigned bits
  }
  deriving (Generic, NFDataX)

data RingState bits = RingState
  { hd :: Unsigned bits
  , tl :: Unsigned bits
  }
  deriving (Generic, NFDataX)

ring ::
  forall dom bits nw nr dat.
  (HiddenClockResetEnable dom, KnownNat bits, KnownNat nw, KnownNat nr, NFDataX dat, nr + 1 <= 2 ^ bits) =>
  SNat bits -> Signal dom (RingReq bits nw nr dat) -> Signal dom (RingResp bits nr dat)
ring SNat req = resp <$> s <*> multiRam (rdAddrs <$> s) writes
  where
    (s, writes) = unbundle $ mealy step RingState {hd = 0, tl = 0} req

    step cur@RingState {hd, tl} RingReq {..} = (RingState {hd = hd', tl = tl'}, (cur, ws))
      where
        hd' = hd + numConvert pop
        addrs = scanl (\a w -> if isJust w then a + 1 else a) tl wdata
        (tl', ws)
          | squash = (hd', repeat Nothing)
          | otherwise = (last addrs, zipWith (fmap . (,)) (init addrs) wdata)

    rdAddrs RingState {hd} = (hd +) . numConvert <$> indicesI @nr

    resp RingState {hd, tl} elems = RingResp {rdata = imap (\i -> orNothing (numConvert i < used)) elems, free = maxBound - used}
      where
        used = tl - hd
