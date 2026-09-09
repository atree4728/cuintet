module Cuintet.Unit.MultiRam (multiRam) where

import Clash.Prelude

multiRam ::
  forall dom bits nw nr dat.
  (HiddenClockResetEnable dom, KnownNat bits, KnownNat nw, KnownNat nr, NFDataX dat) =>
  Signal dom (Vec nr (Unsigned bits)) ->
  Signal dom (Vec nw (Maybe (Unsigned bits, dat))) ->
  Signal dom (Vec nr dat)
multiRam rdAddrs writes = pick <$> lvt <*> banked <*> rdAddrs
  where
    pick l = zipWith $ \banks addr -> banks !! (l !! addr)
    banked = bundle $ (\addr -> bundle (asyncRamPow2 addr <$> unbundle writes)) <$> unbundle rdAddrs
    lvt :: Signal dom (Vec (2 ^ bits) (Index nw))
    lvt = register (repeat 0) (ifoldl newest <$> lvt <*> writes)
    newest acc bank = maybe acc (\(addr, _) -> replace addr bank acc)
