module Cuintet.Util (orNothing, downto) where

import Clash.Annotations.BitRepresentation (BitMask)
import Clash.Prelude

orNothing :: Bool -> a -> Maybe a
orNothing True x = Just x
orNothing False _ = Nothing

-- | Mask covering bits @h@ down to @l@, for 'Clash.Annotations.BitRepresentation.ConstrRepr'.
downto :: Int -> Int -> BitMask
downto h l = (1 `shiftL` (h - l + 1) - 1) `shiftL` l
