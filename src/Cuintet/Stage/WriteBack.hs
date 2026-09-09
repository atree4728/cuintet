module Cuintet.Stage.WriteBack (writeback, WriteBackIn (..), WriteBackOut (..)) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), isLoad)
import Cuintet.Eei (IssueWidth, PRegAddr, XLen)
import Cuintet.Pipeline (Executed (..), pdOf)
import Cuintet.Unit.LoadStore (LoadStoreJob (..), LoadStoreResp (..))
import Cuintet.Unit.Rob (Completed (..), RobDone (..))
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe, isNothing)

data WriteBackIn = WriteBackIn
  { entries :: Upto IssueWidth Executed
  , loadStoreResp :: LoadStoreResp
  , commitUsesCsrFile :: Bool
  }

data WriteBackOut = WriteBackOut
  { issue :: Upto IssueWidth Completed
  , issued :: Bool
  , writes :: Vec IssueWidth (Maybe (PRegAddr, BitVector XLen))
  -- ^ The register file write, now that a result is architectural once it is in the ROB.
  , loadStoreJob :: Maybe LoadStoreJob
  }

writeback :: WriteBackIn -> WriteBackOut
writeback WriteBackIn {..} = WriteBackOut {..}
  where
    loadStoreJob = guard (not commitUsesCsrFile) *> (mkJob =<< Upto.head entries)
    mkJob Executed {..}
      | isNothing exception
      , Just memOp <- ctrl.memOp =
          Just LoadStoreJob {memOp, addr = bitCoerce aluResult, wdata = rs2Data}
      | otherwise = Nothing

    issued = entries.len > 0 && not loadStoreResp.stall && not commitUsesCsrFile

    issue = Upto {len = if issued then entries.len else 0, elems = zipWith completeLane entries.elems lanes}
      where
        lanes = (loadStoreResp.result, loadStoreResp.completed) :> (Nothing, Nothing) :> Nil
        completeLane Executed {..} (result, mem) = Completed {robAddr, robDone}
          where
            robDone = RobDone {value = if isLoad ctrl then fromMaybe (deepErrorX "writeback: load completed without data") result else wbData, ..}

    writes = zipWith mkWrite (Upto.toMaybes issue) entries.elems
    mkWrite completed entry = do
      Completed {robDone} <- completed
      pd <- pdOf entry
      pure (pd, robDone.value)
{-# OPAQUE writeback #-}
