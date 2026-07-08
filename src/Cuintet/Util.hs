module Cuintet.Util where

import Clash.Prelude

orNothing :: Bool -> a -> Maybe a
orNothing True x = Just x
orNothing False _ = Nothing

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

liftAA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
liftAA2 = liftA2 . liftA2

(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftAA2 id

fill :: forall n. (KnownNat n) => Bit -> BitVector n
fill b = pack $ replicate (SNat :: SNat n) b
