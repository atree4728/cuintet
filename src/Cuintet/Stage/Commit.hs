-- | Cm: entering and leaving a trap, and the retire log.
module Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard, mfilter)
import Cuintet.CoreCtrl (OpClass (..))
import Cuintet.Eei (Addr, CommitWidth, Mapping (..), SystemOp (..))
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.Csr (CsrFile (..), CsrReq (..), TrapSpec (..), csrStep)
import Cuintet.Unit.Rob (RobDone (..), RobEntry (..), RobStatic (..), committedMapping, squashes)
import Cuintet.Util (count)
import Data.Maybe (isJust, isNothing)

data CommitIn = CommitIn
  { entries :: Vec CommitWidth (Maybe RobEntry)
  , orderFail :: Vec CommitWidth Bool
  -- ^ From the head of the load queue on.
  }

data CommitOut = CommitOut
  { retired :: Vec CommitWidth (Maybe Retire)
  , mappings :: Vec CommitWidth (Maybe Mapping)
  , redirect :: Maybe Addr
  , pop :: Index (CommitWidth + 1)
  , stores :: Index (CommitWidth + 1)
  , loads :: Index (CommitWidth + 1)
  , squash :: Bool
  }

commit :: CsrFile -> CommitIn -> (CsrFile, CommitOut)
commit csrFile CommitIn {..} = (csrFile', CommitOut {..})
  where
    (entry0, entry1) = vecToTuple entries
    (fail0, fail1) = vecToTuple orderFail

    is opClass e = e.static.opClass == opClass

    -- The head of the load queue is lane 0's load, or lane 1's when lane 0 holds no load.
    refetch0 = mfilter (\e -> is Load e && fail0) entry0
    refetch1 = do
      e0 <- commit0
      guard (not (squashes e0))
      mfilter (\e -> is Load e && if is Load e0 then fail1 else fail0) entry1

    commit0 = guard (isNothing refetch0) *> mfilter (isJust . (.done)) entry0
    commit1 = do
      e0 <- commit0
      guard (not (squashes e0) && isNothing refetch1)
      mfilter (\e -> isJust e.done && isNothing (mkCsrReq e)) entry1
    commits = commit0 :> commit1 :> Nil

    pop = count isJust commits

    refetch = (.static.pc) <$> (refetch0 <|> refetch1)
    squash = maybe False squashes (commit1 <|> commit0) || isJust refetch

    loads = counted (is Load) commits
    stores = counted (is Store) commits

    (csrFile', trapTarget) = csrStep csrFile (mkCsrReq =<< commit0)
    redirect = trapTarget <|> refetch

    mappings = (committedMapping =<<) <$> commits

    retired = (mkRetire =<<) <$> commits
{-# OPAQUE commit #-}

mkCsrReq :: RobEntry -> Maybe CsrReq
mkCsrReq RobEntry {..} = served =<< done
  where
    served RobDone {exception}
      | Just (cause, tval) <- exception = Just $ TrapEnter TrapSpec {epc = static.pc, cause, value = tval}
      | Just SysMret <- static.systemOp = Just TrapReturn
      | otherwise = Nothing

mkRetire :: RobEntry -> Maybe Retire
mkRetire entry@RobEntry {static} = do
  RobDone {exception, value, mem} <- entry.done
  pure
    Retire
      { pc = static.pc
      , instBits = static.instBits
      , rd = (\m -> (m.rdAddr, value)) <$> committedMapping entry
      , mem
      , trap = fst <$> exception
      }

-- | The entries that satisfy @p@ and leave without a trap.
counted :: (RobEntry -> Bool) -> Vec CommitWidth (Maybe RobEntry) -> Index (CommitWidth + 1)
counted p = count (maybe False (\e -> p e && any (isNothing . (.exception)) e.done))
