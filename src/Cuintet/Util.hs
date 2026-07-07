module Cuintet.Util where

import Clash.Prelude

orNothing :: Bool -> a -> Maybe a
orNothing True x = Just x
orNothing False _ = Nothing
