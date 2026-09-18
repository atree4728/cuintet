module Cuintet.Stage.Rename (RenameState (..), initRenameState, RenameIn (..), RenameOut (..), rename) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), isCsr, isLoad, isStore, opClassOf)
import Cuintet.Eei (CommitWidth, DispatchWidth, LoadQueueAddr, Mapping (..), NRegs, PRegAddr, RobAddr, StoreQueueAddr)
import Cuintet.Pipeline (Decoded (..), Renamed (..))
import Cuintet.Unit.Rob (RobStatic (..))
import Cuintet.Util (count)
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
  , sqAllocates :: Index (DispatchWidth + 1)
  , lqAllocates :: Index (DispatchWidth + 1)
  }

rename :: RenameState -> RenameIn -> (RenameState, RenameOut)
rename RenameState {..} RenameIn {..} = (state', RenameOut {..})
  where
    storing = maybe False (isStore . (.ctrl))
    loading = maybe False (isLoad . (.ctrl))
    writing = maybe False (isJust . (.rdAddr))
    accessesCsr = maybe False (isCsr . (.ctrl)) (head entries)

    issued =
      any isJust entries
        && not flush
        && not recovering
        && not serializing
        && (drained || not accessesCsr)
        && count isJust entries
        <= robFree
        && count storing entries
        <= sqFree
    lanes = (guard issued *>) <$> entries

    -- What each lane takes, the lanes before it having taken theirs.
    offsets :: (Num a) => (Maybe Decoded -> Bool) -> a -> Vec DispatchWidth a
    offsets p start = init (scanl (\a e -> if p e then a + 1 else a) start entries)
    freePds = (freeList !!) <$> offsets writing specHead
    robAddrs = offsets (const True) nextRobAddr
    sqAddrs = offsets storing nextSqAddr
    lqAddrs = offsets loading nextLqAddr

    issue = imap renamedLane lanes
    renamedLane i = fmap $ \Decoded {..} ->
      Renamed
        { pdAddr = freePds !! i <$ rdAddr
        , robAddr = robAddrs !! i
        , sqAddr = sqAddrs !! i
        , lqAddr = lqAddrs !! i
        , ps1Addr = specRmt !! rs1Addr
        , ps2Addr = specRmt !! rs2Addr
        , ..
        }

    allocates = imap (\i -> fmap ((robAddrs !! i,) . robStatic (freePds !! i))) lanes
    robStatic pd Decoded {..} =
      RobStatic
        { pc
        , mapping = (\r -> Mapping {rdAddr = r, pdAddr = pd}) <$> rdAddr
        , systemOp = ctrl.systemOp
        , opClass = opClassOf ctrl
        , instBits
        }

    sqAllocates = count storing lanes
    lqAllocates = count loading lanes

    taken = zipWith (\lane pd -> (,pd) <$> ((.rdAddr) =<< lane)) lanes freePds
    specHead' = specHead + count isJust taken

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
