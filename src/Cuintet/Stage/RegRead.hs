module Cuintet.Stage.RegRead (NBypasses, RegReadIn (..), RegReadOut (..), regRead) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Cuintet.CoreCtrl (NExecUnits)
import Cuintet.Eei (IssueWidth, NAluPorts, PRegAddr, XLen)
import Cuintet.Pipeline (Ready (..), Renamed (..))
import Cuintet.Unit.RegFile (ReadPorts)
import Cuintet.Util (bypass)

-- | The units, then each ALU port at WB and at EX.
type NBypasses = NExecUnits + 2 * NAluPorts

data RegReadIn = RegReadIn
  { entries :: Vec IssueWidth (Maybe Renamed)
  , rsData :: Vec ReadPorts (BitVector XLen)
  , bypasses :: Vec NBypasses (Maybe (PRegAddr, BitVector XLen))
  }

data RegReadOut = RegReadOut
  { issue :: Vec IssueWidth (Maybe Ready)
  , rsAddrs :: Vec ReadPorts PRegAddr
  }

regRead :: RegReadIn -> RegReadOut
regRead RegReadIn {..} = RegReadOut {issue, rsAddrs}
  where
    issue = zipWith (\entry rs -> readLane bypasses rs <$> entry) entries (unconcat d2 rsData)
    rsAddrs = concatMap (maybe (repeat 0) (\Renamed {..} -> ps1Addr :> ps2Addr :> Nil)) entries
{-# OPAQUE regRead #-}

readLane :: Vec NBypasses (Maybe (PRegAddr, BitVector XLen)) -> Vec 2 (BitVector XLen) -> Renamed -> Ready
readLane bypasses rsData Renamed {..} = Ready {..}
  where
    (rs1Read, rs2Read) = vecToTuple rsData
    rs1Data = bypass bypasses ps1Addr rs1Read
    rs2Data = bypass bypasses ps2Addr rs2Read
