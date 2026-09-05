-- | Cm: the CSR file, entering and leaving a trap, the register write, and the retire log.
module Cuintet.Stage.Commit (CommitIn (..), CommitOut (..), commit) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, RegAddr, SSWay, SystemOp (..), XLen)
import Cuintet.Pipeline (MaCm (..), Retire (..), destReg)
import Cuintet.Unit.Csr (AccessSpec (..), CsrFile (..), CsrReq (..), CsrResp (..), TrapSpec (..), csrStep)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

newtype CommitIn = CommitIn {entry :: Maybe MaCm}

data CommitOut = CommitOut
  { retired :: Vec SSWay (Maybe Retire)
  , redirect :: Maybe Addr
  , write :: Maybe (RegAddr, BitVector XLen)
  , led :: BitVector XLen
  }

commit :: CsrFile -> CommitIn -> (CsrFile, CommitOut)
commit csrFile CommitIn {..} = (csrFile', commitOut)
  where
    MaCm {..} = fromMaybe (deepErrorX "commit: MA-Cm FIFO is empty") entry
    commitOut = CommitOut {retired = retired :> Nil, redirect, write = retired >>= (.rd), led = csrFile.led}

    valid = isJust entry
    (csrFile', csrResp) = csrStep csrFile csrReq
    csrReq
      | not valid = Nothing
      | Just (cause, value) <- exception = Just $ TrapEnter TrapSpec {epc = pc, ..}
      | Just (SysCsr (src, op, csrAddr)) <- ctrl.systemOp =
          Just $ CsrAccess AccessSpec {csrAddr, op, src, rs1Addr, rs1Data}
      | Just SysMret <- ctrl.systemOp = Just TrapReturn
      | otherwise = Nothing

    wbData' = case csrResp of Just (ReadValue v) -> v; _ -> wbData
    redirect = case csrResp of Just (Redirect v) -> Just v; _ -> Nothing

    retired =
      orNothing valid
        $ Retire
          { pc
          , instBits
          , rd = (,wbData') <$> (destReg =<< entry)
          , mem = completed
          , trap = fst <$> exception
          }
{-# OPAQUE commit #-}
