module Cuintet.Stage.RegRead (RegReadIn (..), RegReadOut (..), regRead) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Cuintet.CoreCtrl (usesRs1, usesRs2)
import Cuintet.Eei (DispatchWidth, XLen)
import Cuintet.Forwarding (Forwarding, NForwards, bypass)
import Cuintet.Pipeline (Ready (..), Renamed (..))
import Cuintet.Upto (Upto (..))
import Data.Maybe (fromMaybe, isJust)

data RegReadIn = RegReadIn
  { entries :: Upto DispatchWidth Renamed
  , rsData :: Vec (2 * DispatchWidth) (BitVector XLen)
  , forwards :: Vec NForwards Forwarding
  , wready :: Bool
  }

newtype RegReadOut = RegReadOut {issue :: Upto DispatchWidth Ready}

regRead :: RegReadIn -> RegReadOut
regRead RegReadIn {..} = RegReadOut {issue}
  where
    ((ready0, ok0), (ready1, ok1)) = vecToTuple $ zipWith (readLane forwards) entries.elems (unconcat d2 rsData)

    issued =
      entries.len
        > 0
        && wready
        && ok0
        && (entries.len < 2 || ok1)

    issue = Upto {len = if issued then entries.len else 0, elems = ready0 :> ready1 :> Nil}
{-# OPAQUE regRead #-}

readLane :: Vec NForwards Forwarding -> Renamed -> Vec 2 (BitVector XLen) -> (Ready, Bool)
readLane forwards Renamed {..} rsData = (Ready {rs1Data = rs1Data', rs2Data = rs2Data', ..}, isJust operands)
  where
    (rs1Read, rs2Read) = vecToTuple rsData
    operands = (,) <$> resolve (usesRs1 ctrl) ps1Addr rs1Read <*> resolve (usesRs2 ctrl) ps2Addr rs2Read
    resolve uses rs regRead' = if uses then bypass forwards rs regRead' else Just regRead'
    (rs1Data', rs2Data') = fromMaybe (rs1Read, rs2Read) operands
