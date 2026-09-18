module Cuintet.Util (orNothing, (<<$>>)) where

import Clash.Prelude

orNothing :: Bool -> a -> Maybe a
orNothing True x = Just x
orNothing False _ = Nothing

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
