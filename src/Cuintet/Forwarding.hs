module Cuintet.Forwarding (NForwards, Forwarding (..), bypass, forwarding, broadcast) where

import Clash.Prelude
import Cuintet.Eei (PRegAddr, XLen)
import Cuintet.Pipeline (Completion, regWrite)

type NForwards = 6 -- MulDiv, LSU, ALU * 2 for EX/WB

data Forwarding
  = Idle
  | Pending PRegAddr
  | Ready PRegAddr (BitVector XLen)
  deriving (Generic, NFDataX)

bypass :: Vec n Forwarding -> PRegAddr -> BitVector XLen -> Maybe (BitVector XLen)
bypass ws rs regRead = foldr pick (Just regRead) ws
  where
    pick (Pending rd) _ | rd == rs = Nothing
    pick (Ready rd v) _ | rd == rs = Just v
    pick _ acc = acc

forwarding :: Maybe PRegAddr -> Maybe (BitVector XLen) -> Forwarding
forwarding rdM value = case rdM of
  Nothing -> Idle
  Just rd -> maybe (Pending rd) (Ready rd) value

broadcast :: Completion -> Forwarding
broadcast = maybe Idle (uncurry Ready) . regWrite
