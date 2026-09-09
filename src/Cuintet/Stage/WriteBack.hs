module Cuintet.Stage.WriteBack (writeback, WriteBackIn (..), WriteBackOut (..)) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), isLoad)
import Cuintet.Eei (IssueWidth, PRegAddr, XLen)
import Cuintet.Pipeline (Completion (..), Executed (..), isSerializing, pdOf)
import Cuintet.Unit.LoadStore (LoadStoreJob (..), LoadStoreResp (..))
import Cuintet.Unit.RegFile (WritePorts)
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe, isJust, isNothing)

data WriteBackIn = WriteBackIn
  { entries :: Upto IssueWidth Executed
  , loadStoreResp :: LoadStoreResp
  , csrWrite :: Maybe (PRegAddr, BitVector XLen)
  , serializingInFlight :: Bool
  }

data WriteBackOut = WriteBackOut
  { issue :: Index (IssueWidth + 1)
  , issued :: Bool
  , serializing :: Bool
  , completions :: Vec WritePorts (Maybe Completion)
  , loadStoreJob :: Maybe LoadStoreJob
  }

writeback :: WriteBackIn -> WriteBackOut
writeback WriteBackIn {..} = WriteBackOut {..}
  where
    (entry0, entry1) = vecToTuple entries.elems

    taken, wanted :: Unsigned 3
    taken = if isJust csrRequest then 1 else 0
    wanted = numConvert entries.len
    lanesFit = taken + wanted <= natToNum @WritePorts

    issued = entries.len > 0 && not loadStoreResp.stall && not serializingInFlight && lanesFit
    issue = if issued then entries.len else 0
    serializing = issued && any (maybe False isSerializing) (Upto.toMaybes entries)

    loadStoreJob = do
      guard (not serializingInFlight)
      Executed {..} <- Upto.head entries
      guard (isNothing exception)
      memOp <- ctrl.memOp
      pure LoadStoreJob {memOp, addr = bitCoerce aluResult, wdata = rs2Data}

    completed entry@Executed {..} (result, mem) =
      Complete robAddr (pdOf entry) RobDone {..}
      where
        value
          | isLoad ctrl = fromMaybe (deepErrorX "writeback: load completed without data") result
          | otherwise = wbData

    csrRequest = uncurry CsrValue <$> csrWrite
    lane0Request = guard (issued && entries.len >= 1) *> Just (completed entry0 (loadStoreResp.result, loadStoreResp.completed))
    lane1Request = guard (issued && entries.len >= 2) *> Just (completed entry1 (Nothing, Nothing))

    (_, completions) = arbitrate (csrRequest :> lane0Request :> lane1Request :> Nil)
{-# OPAQUE writeback #-}

arbitrate ::
  forall n nw a.
  (KnownNat n, KnownNat nw, nw <= n + 1) =>
  Vec n (Maybe a) -> (Vec n Bool, Vec nw (Maybe a))
arbitrate reqs = (grants, slots)
  where
    -- grants = snd $ mapAccumL (\cnt req -> let cnt' = cnt + bool 0 1 (isJust req) in (cnt', cnt < natToNum @nw && isJust req)) 0 reqs
    ahead :: Vec n (Index (n + 1))
    ahead = init (scanl (\acc r -> if isJust r then acc + 1 else acc) 0 reqs)
    grants = zipWith (\r k -> isJust r && k < natToNum @nw) reqs ahead
    slots = imap (\j () -> pick (numConvert j)) (repeat @nw ())
    pick j = foldl (<|>) Nothing (zipWith3 (\r k g -> if g && k == j then r else Nothing) reqs ahead grants)
