{- | WB: applies the register write of an instruction that has committed, and hands the entry out as the core's execution log.

Whether and where to write is decided here, from @rwbEn@ and the @x0@ rule;
@wbData@ is what MA chose to write. 'Cuintet.Pipeline.pendingRd' is the single
definition of "the register this instruction writes", shared with the interlock
in ID.

WB never stalls. That is what lets MA start a memory access without first
checking the MA-WB FIFO for room.
-}
module Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback) where

import Clash.Prelude
import Cuintet.Eei (RegAddr, XLen)
import Cuintet.Pipeline (MaWb (..), destReg)

-- | The instruction leaving the MA-WB FIFO. Its presence means it committed.
newtype WritebackIn = WriteBackIn {entry :: Maybe MaWb}

-- | The instruction that has just retired, and the register write it applies.
data WritebackOut = WriteBackOut
  { retired :: Maybe MaWb
  , write :: Maybe (RegAddr, BitVector XLen)
  }

-- | One clock of WB.
writeback :: WritebackIn -> WritebackOut
writeback WriteBackIn {entry} = WriteBackOut {retired = entry, write}
  where
    write = do
      maWb <- entry
      rdAddr <- destReg maWb
      Just (rdAddr, maWb.wbData)
