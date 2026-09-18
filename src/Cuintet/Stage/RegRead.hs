module Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.Eei (IssueWidth, PRegAddr, XLen)
import Cuintet.Forwarding (NBypasses, bypass)
import Cuintet.Pipeline (Ready (..), Renamed (..))

data RegReadIn = RegReadIn
  { entries :: Vec IssueWidth (Maybe Renamed)
  , rsData :: Vec (2 * IssueWidth) (BitVector XLen)
  , bypasses :: Vec NBypasses (Maybe (PRegAddr, BitVector XLen))
  , wready :: Bool
  }

newtype RegReadOut = RegReadOut {issue :: Vec IssueWidth (Maybe Ready)}

regRead :: RegReadIn -> RegReadOut
regRead RegReadIn {..} =
  RegReadOut
    { issue =
        zipWith
          (\entry rs -> guard wready >> readLane bypasses rs <$> entry)
          entries
          (unconcat d2 rsData)
    }
{-# OPAQUE regRead #-}

readLane :: Vec NBypasses (Maybe (PRegAddr, BitVector XLen)) -> Vec 2 (BitVector XLen) -> Renamed -> Ready
readLane forwards rsData Renamed {..} = Ready {..}
  where
    (rs1Read, rs2Read) = vecToTuple rsData
    rs1Data = bypass forwards ps1Addr rs1Read
    rs2Data = bypass forwards ps2Addr rs2Read
