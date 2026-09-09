-- | Cm: the CSR file, entering and leaving a trap, the register write, and the retire log.
module Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, IssueWidth, PRegAddr, SystemOp (..), XLen)
import Cuintet.Pipeline (Completed (..), Mapping (..), Retire (..), rdOf)
import Cuintet.Unit.Csr (CsrFile (..), CsrReq (..), CsrResp (..), TrapSpec (..), csrStep)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe)

newtype CommitIn = CommitIn {entries :: Upto IssueWidth Completed}

data CommitOut = CommitOut
  { retired :: Vec IssueWidth (Maybe Retire)
  , renamed :: Vec IssueWidth (Maybe Mapping)
  , redirect :: Maybe Addr
  , writes :: Vec IssueWidth (Maybe (PRegAddr, BitVector XLen))
  }

-- | One clock of Cm. A trap is always lane 0's, since EX cancels the younger lanes of a group that traps.
commit :: CsrFile -> CommitIn -> (CsrFile, CommitOut)
commit csrFile CommitIn {..} = (csrFile', CommitOut {..})
  where
    (csrFile', csrResp) = csrStep csrFile (mkCsrReq =<< Upto.head entries)

    readValue = case csrResp of Just (ReadValue v) -> Just v; _ -> Nothing
    redirect = case csrResp of Just (Redirect v) -> Just v; _ -> Nothing

    renamed = (mkMapping =<<) <$> Upto.toMaybes entries

    retired = zipWith (\v e -> mkRetire v <$> e) (readValue :> Nothing :> Nil) (Upto.toMaybes entries)

    writes = zipWith (\v e -> mkWrite v =<< e) (readValue :> Nothing :> Nil) (Upto.toMaybes entries)
{-# OPAQUE commit #-}

mkCsrReq :: Completed -> Maybe CsrReq
mkCsrReq Completed {..}
  | Just (cause, value) <- exception = Just $ TrapEnter TrapSpec {epc = pc, ..}
  | Just (SysCsr spec) <- ctrl.systemOp = Just $ CsrAccess spec rs1Data
  | Just SysMret <- ctrl.systemOp = Just TrapReturn
  | otherwise = Nothing

mkMapping :: Completed -> Maybe Mapping
mkMapping entry@Completed {..} = Mapping {..} <$ rdOf entry

mkRetire :: Maybe (BitVector XLen) -> Completed -> Retire
mkRetire csrValue entry@Completed {..} =
  Retire
    { pc
    , instBits
    , rd = (,fromMaybe wbData csrValue) <$> rdOf entry
    , mem
    , trap = fst <$> exception
    }

mkWrite :: Maybe (BitVector XLen) -> Completed -> Maybe (PRegAddr, BitVector XLen)
mkWrite csrValue entry = (entry.pdAddr, fromMaybe entry.wbData csrValue) <$ rdOf entry
