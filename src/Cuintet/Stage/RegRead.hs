module Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.CoreCtrl (usesRs1, usesRs2)
import Cuintet.Eei (IssueWidth, XLen)
import Cuintet.Forwarding (Forwarding, NForwards, bypass)
import Cuintet.Pipeline (Ready (..), Renamed (..))
import Data.Maybe (fromMaybe, isJust)

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
    lanes = zipWith (\entry rs -> flip (readLane forwards) rs <$> entry) entries (unconcat d2 rsData)

    issued = wready && all (maybe True snd) lanes

    issue = fmap (\r -> guard issued *> (fst <$> r)) lanes
{-# OPAQUE regRead #-}

readLane :: Vec NForwards Forwarding -> Renamed -> Vec 2 (BitVector XLen) -> (Ready, Bool)
readLane forwards Renamed {..} rsData = (Ready {rs1Data = rs1Data', rs2Data = rs2Data', ..}, isJust operands)
  where
    (rs1Read, rs2Read) = vecToTuple rsData
    operands = (,) <$> resolve (usesRs1 ctrl) ps1Addr rs1Read <*> resolve (usesRs2 ctrl) ps2Addr rs2Read
    resolve uses rs regRead' = if uses then bypass forwards rs regRead' else Just regRead'
    (rs1Data', rs2Data') = fromMaybe (rs1Read, rs2Read) operands
