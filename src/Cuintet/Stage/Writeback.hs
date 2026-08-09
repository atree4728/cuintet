{- | WB: applies the register write of an instruction that has committed, and hands the entry out as the core's execution log.

Whether and where to write is decided here, from @rwbEn@ and the @x0@ rule;
@wbData@ is what MA chose to write. 'Cuintet.Pipeline.pendingRd' is the single
definition of "the register this instruction writes", shared with the interlock
in ID.

WB never stalls. That is what lets MA start a memory access without first
checking the MA-WB FIFO for room.
-}
module Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback, initRegFile) where

import Clash.Prelude
import Cuintet.Eei (RegFile)
import Cuintet.Pipeline (MaWb (..), pendingRd)

-- | The instruction leaving the MA-WB FIFO. Its presence means it committed.
newtype WritebackIn = WriteBackIn {entry :: Maybe MaWb}

-- | The instruction that has just retired; the core's execution log.
newtype WritebackOut = WriteBackOut {retired :: Maybe MaWb}

-- | @x0@ reads as zero; the rest are undefined until written.
initRegFile :: RegFile
initRegFile = zeroBits :> replicate d31 (deepErrorX "register uninitialized")

-- | One clock of WB.
writeback :: RegFile -> WritebackIn -> (RegFile, WritebackOut)
writeback regFile WriteBackIn {entry} = (regFile', WriteBackOut {retired = entry})
  where
    regFile' = case entry of
      Just maWb | Just rdAddr <- pendingRd maWb -> replace rdAddr maWb.wbData regFile
      _ -> regFile
