-- | WB: nothing but the arbitration of the write ports.
module Cuintet.Stage.WriteBack (writeback, WriteBackIn (..), WriteBackOut (..)) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard)
import Cuintet.Completion (Completion (..))
import Cuintet.CoreCtrl (Wakeup (..), opClassOf, wakeup)
import Cuintet.Eei (IssueWidth, PRegAddr, WriteBackWidth, XLen)
import Cuintet.Pipeline (Executed (..))
import Cuintet.Unit.Rob (RobDone (..))
import Data.Bool (bool)
import Data.Maybe (isJust)

data WriteBackIn = WriteBackIn
  { entries :: Vec IssueWidth (Maybe Executed)
  , mulDivDone :: Maybe Completion
  , loadDone :: Maybe Completion
  , csrWrite :: Maybe (PRegAddr, BitVector XLen)
  }

data WriteBackOut = WriteBackOut
  { issued :: Bool
  , completions :: Vec WriteBackWidth (Maybe Completion)
  , mulDivGranted :: Bool
  , loadGranted :: Bool
  }

writeback :: WriteBackIn -> WriteBackOut
writeback WriteBackIn {..} = WriteBackOut {..}
  where
    (entry0, entry1) = vecToTuple entries

    taken, wanted :: Unsigned 3
    taken = sum $ bool 0 1 . isJust <$> (csrRequest :> loadDone :> mulDivDone :> Nil)
    wanted = sum $ bool 0 1 . isJust <$> entries

    issued = wanted > 0 && taken + wanted <= natToNum @WriteBackWidth

    completed entry@Executed {..} =
      Complete entry.robAddr pd RobDone {exception, mispredicted, value = entry.wbData, mem}
      where
        pd = guard (wakeup (opClassOf ctrl) /= AtCommit) *> pdAddr

    csrRequest = uncurry CsrValue <$> csrWrite
    port0Request = guard issued *> (completed <$> entry0)
    port1Request = guard issued *> (completed <$> entry1)

    (grants, completions) = arbitrate (csrRequest :> loadDone :> mulDivDone :> port0Request :> port1Request :> Nil)
    loadGranted = grants !! (1 :: Index 4)
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
