module Cuintet.Stage.WriteBack (writeback, WriteBackIn (..), WriteBackOut (..)) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.CoreCtrl (ExecUnit (..), InstCtrl (..), unitOf)
import Cuintet.Eei (IssueWidth, PRegAddr, XLen)
import Cuintet.Pipeline (Completion (..), Executed (..), isSerializing, pdOf)
import Cuintet.Unit.LoadStore (LoadStoreJob (..))
import Cuintet.Unit.RegFile (WritePorts)
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Bool (bool)
import Data.Maybe (isJust, isNothing)

data WriteBackIn = WriteBackIn
  { entries :: Upto IssueWidth Executed
  , mulDivDone :: Maybe Completion
  , loadStoreBusy :: Bool
  , loadStoreDone :: Maybe Completion
  , csrWrite :: Maybe (PRegAddr, BitVector XLen)
  , serializingInFlight :: Bool
  }

data WriteBackOut = WriteBackOut
  { issue :: Index (IssueWidth + 1)
  , issued :: Bool
  , serializing :: Bool
  , completions :: Vec WritePorts (Maybe Completion)
  , loadStoreJob :: Maybe LoadStoreJob
  , mulDivGranted :: Bool
  , loadStoreGranted :: Bool
  }

dispatched :: Executed -> Bool
dispatched entry =
  isNothing entry.exception && case unitOf entry.ctrl of
    Alu _ -> False
    _ -> True

writeback :: WriteBackIn -> WriteBackOut
writeback WriteBackIn {..} = WriteBackOut {..}
  where
    (entry0, entry1) = vecToTuple entries.elems

    taken, wanted :: Unsigned 3
    taken = sum $ bool 0 1 . isJust <$> (csrRequest :> loadStoreDone :> mulDivDone :> Nil)
    wanted = bool 0 1 (entries.len >= 1 && not (dispatched entry0)) + bool 0 1 (entries.len >= 2)

    memJob = do
      entry@Executed {..} <- Upto.head entries
      guard (isNothing exception)
      memOp <- ctrl.memOp
      pure LoadStoreJob {memOp, addr = bitCoerce aluResult, wdata = rs2Data, pdAddr = pdOf entry, robAddr, mispredicted}

    memOk = case memJob of
      Nothing -> True
      Just _ -> not loadStoreBusy

    issued = entries.len > 0 && memOk && not serializingInFlight && taken + wanted <= natToNum @WritePorts
    loadStoreJob = guard issued *> memJob
    issue = if issued then entries.len else 0
    serializing = issued && any (maybe False isSerializing) (Upto.toMaybes entries)

    completed entry@Executed {..} =
      Complete entry.robAddr (pdOf entry) RobDone {exception, mispredicted, value = entry.wbData, mem = Nothing}

    csrRequest = uncurry CsrValue <$> csrWrite
    lane0Request = guard (issued && entries.len >= 1 && not (dispatched entry0)) *> Just (completed entry0)
    lane1Request = guard (issued && entries.len >= 2) *> Just (completed entry1)

    (grants, completions) = arbitrate (csrRequest :> loadStoreDone :> mulDivDone :> lane0Request :> lane1Request :> Nil)
    loadStoreGranted = grants !! (1 :: Index 4)
    mulDivGranted = grants !! (2 :: Index 5)
{-# OPAQUE writeback #-}

arbitrate ::
  forall n nw a.
  (KnownNat n, KnownNat nw, nw <= n + 1) =>
  Vec n (Maybe a) -> (Vec n Bool, Vec nw (Maybe a))
arbitrate reqs = (grants, slots)
  where
    ahead :: Vec n (Index (n + 1))
    ahead = init (scanl (\acc r -> if isJust r then acc + 1 else acc) 0 reqs)
    grants = zipWith (\r k -> isJust r && k < natToNum @nw) reqs ahead
    slots = imap (\j () -> pick (numConvert j)) (repeat @nw ())
    pick j = foldl (<|>) Nothing (zipWith3 (\r k g -> if g && k == j then r else Nothing) reqs ahead grants)
