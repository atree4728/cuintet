module Cuintet.Util (orNothing, (<<$>>), count, isOlder, oldest, bypass) where

import Clash.Prelude
import Data.Bool (bool)

orNothing :: Bool -> a -> Maybe a
orNothing True x = Just x
orNothing False _ = Nothing

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

count :: (KnownNat n, Num c) => (a -> Bool) -> Vec n a -> c
count p = sum . map (bool 0 1 . p)

-- | Whether @a@ comes before @b@ in a ring whose oldest entry is @hd@.
isOlder :: (KnownNat m) => Unsigned m -> Unsigned m -> Unsigned m -> Bool
isOlder a b hd = a - hd < b - hd

-- | The entry nearest @hd@ in a ring.
oldest :: (KnownNat m) => Unsigned m -> Vec (n + 1) (Maybe (Unsigned m, a)) -> Maybe (Unsigned m, a)
oldest hd = fmap snd . fold pick . map (fmap (\e@(addr, _) -> (addr - hd, e)))
  where
    pick l r = case (l, r) of
      (Just (x, _), Just (y, _)) | y < x -> r
      (Nothing, _) -> r
      _ -> l

-- | The value at @k@, as the last matching write leaves it.
bypass :: (Eq k) => Vec n (Maybe (k, v)) -> k -> v -> v
bypass writes k stored = foldl pick stored writes
  where
    pick _ (Just (k', v)) | k' == k = v
    pick acc _ = acc
