module Cuintet.Stage.Rename (RenameState (..), initRenameState, RenameIn (..), RenameOut (..), rename) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (CommitWidth, DispatchWidth, Mapping (..), NRegs, PRegAddr, RobAddr)
import Cuintet.Pipeline (Decoded (..), Renamed (..), validRdOf)
import Cuintet.Unit.Rob (RobStatic (..))
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
  { entries :: Upto DispatchWidth Decoded
  , committed :: Vec CommitWidth (Maybe Mapping)
  , nextRobAddr :: RobAddr
  , robFree :: RobAddr
  , flush :: Bool
  , drained :: Bool
  -- ^ Whether the ROB is empty, so nothing more will reach Cm.
  , wready :: Bool
  }

data RenameOut = RenameOut
  { issue :: Upto DispatchWidth Renamed
  , allocates :: Upto DispatchWidth RobStatic
  }

rename :: RenameState -> RenameIn -> (RenameState, RenameOut)
rename RenameState {..} RenameIn {..} = (state', RenameOut {..})
  where
    (decoded0, decoded1) = vecToTuple entries.elems

    issued = entries.len > 0 && wready && not flush && not recovering && numConvert entries.len <= robFree

    rdAddr0 = validRdOf decoded0
    rdAddr1 = validRdOf decoded1

    freePd0 = freeList !! specHead
    freePd1 = freeList !! (specHead + if isJust rdAddr0 then 1 else 0)

    pdAddr0 = freePd0 <$ rdAddr0
    pdAddr1 = freePd1 <$ rdAddr1

    robAddr0 = nextRobAddr
    robAddr1 = nextRobAddr + 1

    robStatic Decoded {..} rd pd =
      RobStatic
        { pc
        , mapping = (\r -> Mapping {rdAddr = r, pdAddr = pd}) <$> rd
        , systemOp = ctrl.systemOp
        , instBits
        }

    len = if issued then entries.len else 0
    issue = Upto {len, elems = renamedLane decoded0 pdAddr0 robAddr0 :> renamedLane decoded1 pdAddr1 robAddr1 :> Nil}
    renamedLane Decoded {..} pdAddr robAddr = Renamed {..}
      where
        ps1Addr = specRmt !! rs1Addr
        ps2Addr = specRmt !! rs2Addr

    allocates = Upto {len, elems = robStatic decoded0 rdAddr0 freePd0 :> robStatic decoded1 rdAddr1 freePd1 :> Nil}

    taken = (if issue.len >= 1 then (,freePd0) <$> rdAddr0 else Nothing) :> (if issue.len >= 2 then (,freePd1) <$> rdAddr1 else Nothing) :> Nil
    takenCnt = sum $ bool 0 1 . isJust <$> taken
    specHead' = specHead + takenCnt

    specUpdate rmt = maybe rmt $ \(rd, pd) -> replace rd pd rmt
    specRmt' = foldl specUpdate specRmt taken

    archUpdate (fl, h, rmt) = maybe (fl, h, rmt) $ \Mapping {..} -> (replace h (rmt !! rdAddr) fl, h + 1, replace rdAddr pdAddr rmt)
    (freeList', archHead', archRmt') = foldl archUpdate (freeList, archHead, archRmt) committed

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
