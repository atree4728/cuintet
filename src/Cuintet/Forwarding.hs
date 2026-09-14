module Cuintet.Forwarding (NForwards, Forwarding (..), bypass, forwarding, broadcast, dest) where

import Clash.Prelude
import Cuintet.Completion (Completion, regWrite)
import Cuintet.Eei (PRegAddr, XLen)
import Data.Maybe (fromMaybe)

type NForwards = 6 -- MulDiv, LSU, ALU * 2 for EX/WB

data Forwarding
  = Idle
  | Ready PRegAddr (BitVector XLen)
  deriving (Generic, NFDataX)

bypass :: Vec n Forwarding -> PRegAddr -> BitVector XLen -> BitVector XLen
bypass ws rs regRead = foldr pick regRead ws
  where
    pick (Ready rd v) _ | rd == rs = v
    pick _ acc = acc

forwarding :: Maybe PRegAddr -> Maybe (BitVector XLen) -> Forwarding
forwarding rdM value = fromMaybe Idle (Ready <$> rdM <*> value)

broadcast :: Completion -> Forwarding
broadcast = maybe Idle (uncurry Ready) . regWrite

dest :: Forwarding -> Maybe PRegAddr
dest (Ready rd _) = Just rd
dest Idle = Nothing
