module Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.Eei (IssueWidth, XLen)
import Cuintet.Forwarding (Forwarding, NForwards, bypass)
import Cuintet.Pipeline (Ready (..), Renamed (..))

data RegReadIn = RegReadIn
  { entries :: Vec IssueWidth (Maybe Renamed)
  , rsData :: Vec (2 * IssueWidth) (BitVector XLen)
  , forwards :: Vec NForwards Forwarding
  , wready :: Bool
  }

newtype RegReadOut = RegReadOut {issue :: Vec IssueWidth (Maybe Ready)}

regRead :: RegReadIn -> RegReadOut
regRead RegReadIn {..} = RegReadOut {issue}
  where
    issue = zipWith (\entry rs -> guard wready *> (readLane forwards rs <$> entry)) entries (unconcat d2 rsData)
{-# OPAQUE regRead #-}

readLane :: Vec NForwards Forwarding -> Vec 2 (BitVector XLen) -> Renamed -> Ready
readLane forwards rsData Renamed {..} = Ready {rs1Data = bypass forwards ps1Addr rs1Read, rs2Data = bypass forwards ps2Addr rs2Read, ..}
  where
    (rs1Read, rs2Read) = vecToTuple rsData
