-- | Cm: the CSR file, entering and leaving a trap, the register write, and the retire log.
module Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.Monad (guard, mfilter)
import Cuintet.CoreCtrl (OpClass (..))
import Cuintet.Eei (Addr, CommitWidth, Mapping (..), PRegAddr, SystemOp (..), XLen)
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.Csr (CsrFile (..), CsrReq (..), CsrResp (..), TrapSpec (..), csrStep)
import Cuintet.Unit.Rob (RobDone (..), RobEntry (..), RobStatic (..), committedMapping, squashes)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, isJust, isNothing)

data CommitIn = CommitIn
  { entries :: Vec CommitWidth (Maybe RobEntry)
  , orderFail :: Vec CommitWidth Bool
  -- ^ From the head of the load queue on.
  }

data CommitOut = CommitOut
  { retired :: Vec CommitWidth (Maybe Retire)
  , renamed :: Vec CommitWidth (Maybe Mapping)
  , redirect :: Maybe Addr
  , csrWrite :: Maybe (PRegAddr, BitVector XLen)
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

    isLoadEntry e = e.static.opClass == Load

    -- The head of the load queue is lane 0's load, or lane 1's when lane 0 holds no load.
    refetch0 = mfilter (\e -> isLoadEntry e && fail0) entry0
    refetch1 = do
      e0 <- commit0
      guard (not (squashes e0))
      mfilter (\e -> isLoadEntry e && if isLoadEntry e0 then fail1 else fail0) entry1

    commit0 = guard (isNothing refetch0) *> mfilter (isJust . (.done)) entry0
    commit1 = do
      e0 <- commit0
      guard (not (squashes e0) && isNothing refetch1)
      mfilter (\e -> isJust e.done && isNothing (mkCsrReq e)) entry1
    commits = commit0 :> commit1 :> Nil

    pop
      | isJust commit1 = 2
      | isJust commit0 = 1
      | otherwise = 0

    refetch = (.static.pc) <$> (refetch0 <|> refetch1)
    squash = maybe False squashes (commit1 <|> commit0) || isJust refetch

    loads = counted Load commits
    stores = counted Store commits

    (csrFile', csrResp) = csrStep csrFile (mkCsrReq =<< commit0)

    readValue = case csrResp of Just (ReadValue v) -> Just v; _ -> Nothing
    redirect = (case csrResp of Just (Redirect v) -> Just v; _ -> Nothing) <|> refetch

    renamed = (committedMapping =<<) <$> commits

    retired = zipWith (\v e -> mkRetire v =<< e) (readValue :> Nothing :> Nil) commits

    csrWrite = do
      value <- readValue
      Mapping {pdAddr} <- committedMapping =<< commit0
      pure (pdAddr, value)
{-# OPAQUE commit #-}

mkCsrReq :: RobEntry -> Maybe CsrReq
mkCsrReq RobEntry {..} = served =<< done
  where
    served RobDone {exception, value}
      | Just (cause, tval) <- exception = Just $ TrapEnter TrapSpec {epc = static.pc, cause, value = tval}
      | Just (SysCsr spec) <- static.systemOp = Just $ CsrAccess spec value
      | Just SysMret <- static.systemOp = Just TrapReturn
      | otherwise = Nothing

mkRetire :: Maybe (BitVector XLen) -> RobEntry -> Maybe Retire
mkRetire csrValue entry@RobEntry {static} = do
  RobDone {exception, value, mem} <- entry.done
  pure
    Retire
      { pc = static.pc
      , instBits = static.instBits
      , rd = (\m -> (m.rdAddr, fromMaybe value csrValue)) <$> committedMapping entry
      , mem
      , trap = fst <$> exception
      }

counted :: OpClass -> Vec CommitWidth (Maybe RobEntry) -> Index (CommitWidth + 1)
counted opClass = sum . fmap (bool 0 1 . maybe False retiring)
  where
    retiring RobEntry {..} = static.opClass == opClass && any (isNothing . (.exception)) done
