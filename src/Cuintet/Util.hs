module Cuintet.Util where

import Clash.Prelude

orNothing :: Bool -> a -> Maybe a
orNothing True x = Just x
orNothing False _ = Nothing

fill :: forall n. (KnownNat n) => Bit -> BitVector n
fill b = pack $ replicate (SNat :: SNat n) b
