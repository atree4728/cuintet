-- | Cm: the CSR file, entering and leaving a trap, the register write, and the retire log.
module Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit) where

import Clash.Prelude
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Cuintet.Eei (Addr, IssueWidth, Mapping (..), PRegAddr, SystemOp (..), XLen)
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.Csr (CsrFile (..), CsrReq (..), CsrResp (..), TrapSpec (..), csrStep)
import Cuintet.Unit.Rob (RobDone (..), RobEntry (..), RobStatic (..), committedMapping, squashes)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe, isJust)

newtype CommitIn = CommitIn {entries :: Upto IssueWidth RobEntry}

data CommitOut = CommitOut
  { retired :: Vec IssueWidth (Maybe Retire)
  , renamed :: Vec IssueWidth (Maybe Mapping)
  , redirect :: Maybe Addr
  , csrWrite :: Maybe (PRegAddr, BitVector XLen)
  , pop :: Index (IssueWidth + 1)
  , squash :: Bool
  }

commit :: CsrFile -> CommitIn -> (CsrFile, CommitOut)
commit csrFile CommitIn {..} = (csrFile', CommitOut {..})
  where
    (entry0, entry1) = vecToTuple entries.elems

    pop
      | entries.len >= 2, isJust entry0.done, not (squashes entry0), isJust entry1.done = 2
      | entries.len >= 1, isJust entry0.done = 1
      | otherwise = 0

    commits = Upto {len = pop, elems = entries.elems}

    squash = case pop of
      2 -> squashes entry1
      1 -> squashes entry0
      _ -> False

    (csrFile', csrResp) = csrStep csrFile (mkCsrReq =<< Upto.head commits)

    readValue = case csrResp of Just (ReadValue v) -> Just v; _ -> Nothing
    redirect = case csrResp of Just (Redirect v) -> Just v; _ -> Nothing

    renamed = (committedMapping =<<) <$> Upto.toMaybes commits

    retired = zipWith (\v e -> mkRetire v =<< e) (readValue :> Nothing :> Nil) (Upto.toMaybes commits)

    csrWrite = do
      value <- readValue
      Mapping {pdAddr} <- committedMapping =<< Upto.head commits
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
