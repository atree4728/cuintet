module Cuintet.Stage.Rename (RenameState (..), initRenameState, RenameIn (..), RenameOut (..), rename) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), isCsrRead, isLoad, isStore, opClassOf)
import Cuintet.Eei (CommitWidth, DispatchWidth, LoadQueueAddr, Mapping (..), NRegs, PRegAddr, RobAddr, StoreQueueAddr)
import Cuintet.Pipeline (Decoded (..), Renamed (..))
import Cuintet.Unit.Rob (RobStatic (..))
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
  , serializing :: Bool
  -- ^ Set by a CSR instruction, cleared once it has retired.
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
    , serializing = False
    }

data RenameIn = RenameIn
  { entries :: Vec DispatchWidth (Maybe Decoded)
  , committed :: Vec CommitWidth (Maybe Mapping)
  , nextRobAddr :: RobAddr
  , robFree :: RobAddr
  , nextSqAddr :: StoreQueueAddr
  , sqFree :: StoreQueueAddr
  , nextLqAddr :: LoadQueueAddr
  , flush :: Bool
  , drained :: Bool
  -- ^ Whether the ROB is empty, so nothing more will reach Cm.
  }

data RenameOut = RenameOut
  { issue :: Vec DispatchWidth (Maybe Renamed)
  , allocates :: Vec DispatchWidth (Maybe (RobAddr, RobStatic))
  }

rename :: RenameState -> RenameIn -> (RenameState, RenameOut)
rename RenameState {..} RenameIn {..} = (state', RenameOut {..})
  where
    (decoded0, decoded1) = vecToTuple entries

    storing = maybe False (isStore . (.ctrl))
    loading = maybe False (isLoad . (.ctrl))
    accessesCsr = maybe False (isCsrRead . (.ctrl)) decoded0

    issued =
      any isJust entries
        && not flush
        && not recovering
        && not serializing
        && (drained || not accessesCsr)
        && sum (bool 0 1 . isJust <$> entries)
        <= robFree
        && sum (bool 0 1 . storing <$> entries)
        <= sqFree
    (lane0, lane1) = vecToTuple $ (guard issued *>) <$> entries

    rdAddr0 = (.rdAddr) =<< decoded0
    rdAddr1 = (.rdAddr) =<< decoded1

    freePd0 = freeList !! specHead
    freePd1 = freeList !! (specHead + if isJust rdAddr0 then 1 else 0)

    pdAddr0 = freePd0 <$ rdAddr0
    pdAddr1 = freePd1 <$ rdAddr1

    robAddr0 = nextRobAddr
    robAddr1 = nextRobAddr + 1

    sqAddr0 = nextSqAddr
    sqAddr1 = nextSqAddr + bool 0 1 (storing decoded0)

    lqAddr0 = nextLqAddr
    lqAddr1 = nextLqAddr + bool 0 1 (loading decoded0)

    robStatic rd pd Decoded {..} =
      RobStatic
        { pc
        , mapping = (\r -> Mapping {rdAddr = r, pdAddr = pd}) <$> rd
        , systemOp = ctrl.systemOp
        , opClass = opClassOf ctrl
        , instBits
        }

    issue = (renamedLane pdAddr0 robAddr0 sqAddr0 lqAddr0 <$> lane0) :> (renamedLane pdAddr1 robAddr1 sqAddr1 lqAddr1 <$> lane1) :> Nil
    renamedLane pdAddr robAddr sqAddr lqAddr Decoded {..} = Renamed {..}
      where
        ps1Addr = specRmt !! rs1Addr
        ps2Addr = specRmt !! rs2Addr

    allocates = ((robAddr0,) . robStatic rdAddr0 freePd0 <$> lane0) :> ((robAddr1,) . robStatic rdAddr1 freePd1 <$> lane1) :> Nil

    taken = (lane0 *> ((,freePd0) <$> rdAddr0)) :> (lane1 *> ((,freePd1) <$> rdAddr1)) :> Nil
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
        , serializing = (issued && accessesCsr) || (serializing && not drained)
        }

    restore = recovering && drained && not flush
{-# OPAQUE rename #-}
