module Cuintet.Stage.Rename (RenameState (..), initRenameState, RenameIn (..), RenameOut (..), rename) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Cuintet.Eei (IssueWidth, PRegAddr)
import Cuintet.Pipeline (Decoded (..), Mapping (..), Renamed (..), validRdOf)
import Cuintet.Unit.RegFile (NRegs)
import Cuintet.Upto (Upto (..))
import Data.Bool (bool)
import Data.Maybe (isJust)

data RenameState = RenameState
  { specRmt :: Vec NRegs PRegAddr
  , archRmt :: Vec NRegs PRegAddr
  , freeList :: Vec NRegs PRegAddr
  -- ^ A ring that is always full: @[archHead, specHead)@ is taken, @[specHead, archHead)@ is free.
  , specHead :: Unsigned 5
  , archHead :: Unsigned 5
  , recovering :: Bool
  -- ^ Set by a flush, cleared once the stages behind EX have drained and @specRmt@ has been restored.
  }
  deriving (Generic, NFDataX)

initRenameState :: RenameState
initRenameState =
  RenameState
    { specRmt = iterateI (+ 1) 0
    , archRmt = iterateI (+ 1) 0
    , freeList = iterateI (+ 1) 32
    , specHead = 0
    , archHead = 0
    , recovering = False
    }

data RenameIn = RenameIn
  { entries :: Upto IssueWidth Decoded
  , committed :: Vec IssueWidth (Maybe Mapping)
  , flush :: Bool
  , drained :: Bool
  -- ^ Whether the Executed and Completed FIFOs are both empty, so nothing more will reach Cm.
  , wready :: Bool
  }

newtype RenameOut = RenameOut {issue :: Upto IssueWidth Renamed}

rename :: RenameState -> RenameIn -> (RenameState, RenameOut)
rename RenameState {..} RenameIn {..} = (state', RenameOut {issue})
  where
    (decoded0, decoded1) = vecToTuple entries.elems

    issued = entries.len > 0 && wready && not flush && not recovering

    rdAddr0 = validRdOf decoded0
    rdAddr2 = validRdOf decoded1

    pdAddr0 = freeList !! specHead
    pdAddr1 = freeList !! (specHead + if isJust rdAddr0 then 1 else 0)

    oldPdAddr0 = specRmt !! decoded0.rdAddr
    oldPdAddr1
      | rdAddr0 == Just decoded1.rdAddr = pdAddr0
      | otherwise = specRmt !! decoded1.rdAddr

    issue =
      Upto
        { len = if issued then entries.len else 0
        , elems = renamedLane decoded0 pdAddr0 oldPdAddr0 :> renamedLane decoded1 pdAddr1 oldPdAddr1 :> Nil
        }
    renamedLane Decoded {..} pdAddr oldPdAddr = Renamed {ps1Addr = specRmt !! rs1Addr, ps2Addr = specRmt !! rs2Addr, ..}

    specRmtUpdate rmt = maybe rmt (\(rd, pd) -> replace rd pd rmt)
    archRmtUpdate rmt = maybe rmt (\x -> replace x.rdAddr x.pdAddr rmt)
    freePd (l, h) = maybe (l, h) (\x -> (replace h x.oldPdAddr l, h + 1))

    taken = (if issue.len >= 1 then (,pdAddr0) <$> rdAddr0 else Nothing) :> (if issue.len >= 2 then (,pdAddr1) <$> rdAddr2 else Nothing) :> Nil
    takenCnt = sum $ bool 0 1 . isJust <$> taken
    specHead' = specHead + takenCnt
    specRmt' = foldl specRmtUpdate specRmt taken
    archRmt' = foldl archRmtUpdate archRmt committed
    (freeList', archHead') = foldl freePd (freeList, archHead) committed

    state' =
      RenameState
        { specRmt = if restore then archRmt' else specRmt'
        , archRmt = archRmt'
        , freeList = freeList'
        , specHead = if restore then archHead' else specHead'
        , archHead = archHead'
        , recovering = flush || (recovering && not drained)
        }

    restore = recovering && drained && not flush
{-# OPAQUE rename #-}
