-- | A queue entered and left in order.
module Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring) where

import Clash.Prelude
import Cuintet.Unit.MultiRam (multiRam)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe)

data RingReq bits nw nr dat = RingReq
  { wdata :: Upto nw dat
  , pop :: Index (nr + 1)
  , squash :: Bool
  }
  deriving (Generic, NFDataX)

data RingResp bits nr dat = RingResp
  { rdata :: Upto nr dat
  , hd :: Unsigned bits
  , tl :: Unsigned bits
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
  (HiddenClockResetEnable dom, KnownNat bits, KnownNat nw, KnownNat nr, NFDataX dat, nw + 1 <= 2 ^ bits, nr + 1 <= 2 ^ bits) =>
  SNat bits -> Signal dom (RingReq bits nw nr dat) -> Signal dom (RingResp bits nr dat)
ring SNat req = resp <$> s <*> multiRam (rdAddrs <$> s) writes
  where
    (s, writes) = unbundle $ mealy step RingState {hd = 0, tl = 0} req

    step cur@RingState {hd, tl} RingReq {..} = (RingState {hd = hd', tl = tl'}, (cur, ws))
      where
        hd' = hd + numConvert pop
        (tl', ws)
          | squash = (hd', repeat Nothing)
          | otherwise = (tl + numConvert wdata.len, imap (\i -> fmap (tl + numConvert i,)) (Upto.toMaybes wdata))

    rdAddrs RingState {hd} = (hd +) . numConvert <$> indicesI @nr

    resp RingState {hd, tl} elems = RingResp {rdata = Upto {len, elems}, hd, tl, free = maxBound - used}
      where
        used = tl - hd
        len = fromMaybe maxBound (maybeNumConvert used)
