module Cuintet.Stage.Rename (RenameIn (..), RenameOut (..), rename) where

import Clash.Prelude
import Cuintet.Eei (IssueWidth)
import Cuintet.Pipeline (Decoded (..), Renamed (..))
import Cuintet.Upto (Upto (..))

data RenameIn = RenameIn
  { entries :: Upto IssueWidth Decoded
  , flush :: Bool
  , wready :: Bool
  }

newtype RenameOut = RenameOut {issue :: Upto IssueWidth Renamed}

rename :: RenameIn -> RenameOut
rename RenameIn {..} = RenameOut {issue}
  where
    issued = entries.len > 0 && wready && not flush

    issue = Upto {len = if issued then entries.len else 0, elems = pass <$> entries.elems}
    pass Decoded {..} = Renamed {ps1Addr = zeroExtend rs1Addr, ps2Addr = zeroExtend rs2Addr, pdAddr = zeroExtend rdAddr, oldPdAddr = zeroExtend rdAddr, ..}
{-# OPAQUE rename #-}
