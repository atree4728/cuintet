-- | Cm: the CSR file, entering and leaving a trap, the register write, and the retire log.
module Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, IssueWidth, SystemOp (..), XLen)
import Cuintet.Pipeline (Completed (..), Retire (..), rdOf)
import Cuintet.Unit.Csr (AccessSpec (..), CsrFile (..), CsrReq (..), CsrResp (..), TrapSpec (..), csrStep)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe)

newtype CommitIn = CommitIn {entries :: Upto IssueWidth Completed}

data CommitOut = CommitOut
  { retired :: Vec IssueWidth (Maybe Retire)
  , redirect :: Maybe Addr
  , led :: BitVector XLen
  }

-- | One clock of Cm. A trap is always lane 0's, since EX cancels the younger lanes of a group that traps.
commit :: CsrFile -> CommitIn -> (CsrFile, CommitOut)
commit csrFile CommitIn {..} = (csrFile', CommitOut {retired, redirect, led = csrFile.led})
  where
    (csrFile', csrResp) = csrStep csrFile (mkCsrReq =<< Upto.head entries)

    readValue = case csrResp of Just (ReadValue v) -> Just v; _ -> Nothing
    redirect = case csrResp of Just (Redirect v) -> Just v; _ -> Nothing

    retired = zipWith (\v e -> mkRetire v <$> e) (readValue :> Nothing :> Nil) (Upto.toMaybes entries)
{-# OPAQUE commit #-}

mkCsrReq :: Completed -> Maybe CsrReq
mkCsrReq Completed {..}
  | Just (cause, value) <- exception = Just $ TrapEnter TrapSpec {epc = pc, ..}
  | Just (SysCsr (src, op, csrAddr)) <- ctrl.systemOp = Just $ CsrAccess AccessSpec {csrAddr, op, src, rs1Addr, rs1Data}
  | Just SysMret <- ctrl.systemOp = Just TrapReturn
  | otherwise = Nothing

-- | The retire log of one lane. The CSR read value, which only lane 0 can carry, arrives too late for @wbData@.
mkRetire :: Maybe (BitVector XLen) -> Completed -> Retire
mkRetire csrValue entry@Completed {..} =
  Retire
    { pc
    , instBits
    , rd = (,fromMaybe wbData csrValue) <$> rdOf entry
    , mem
    , trap = fst <$> exception
    }
