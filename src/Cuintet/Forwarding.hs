module Cuintet.Forwarding (Bypass (..), Forwarding (..), bypass, forwarding) where

import Clash.Prelude
import Cuintet.Eei (RegAddr, XLen)

data Bypass = Bypass
  { rd :: RegAddr
  , value :: Maybe (BitVector XLen)
  }
  deriving (Generic, NFDataX)

data Forwarding
  = Idle
  | Pending RegAddr
  | Ready RegAddr (BitVector XLen)
  deriving (Generic, NFDataX)

bypass :: Vec n Forwarding -> RegAddr -> BitVector XLen -> Maybe (BitVector XLen)
bypass ws rs regRead = foldr pick (Just regRead) ws
  where
    pick (Pending rd) _ | rd == rs = Nothing
    pick (Ready rd v) _ | rd == rs = Just v
    pick _ acc = acc

forwarding :: Maybe RegAddr -> Maybe (BitVector XLen) -> Forwarding
forwarding rdM value = case rdM of
  Nothing -> Idle
  Just rd -> maybe (Pending rd) (Ready rd) value
