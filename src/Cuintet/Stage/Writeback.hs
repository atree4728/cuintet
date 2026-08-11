{- | WB: turns an instruction that has committed into a register write, and hands the entry out as the core's execution log.

Whether and where to write is decided here, from @rwbEn@ and the @x0@ rule;
@wbData@ is what MA chose to write. 'Cuintet.Pipeline.destReg' is the single
definition of "the register this instruction writes", shared with the interlock
in ID. The write itself lands in 'Cuintet.RegFile.regFile' at the next clock
edge.

WB never stalls. That is what lets MA start a memory access without first
checking the MA-WB FIFO for room.
-}
module Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback) where

import Clash.Prelude
import Cuintet.Eei (RegAddr, XLen)
import Cuintet.Pipeline (MaWb (..), destReg)

-- | The instruction leaving the MA-WB FIFO. Its presence means it committed.
newtype WritebackIn = WriteBackIn {entry :: Maybe MaWb}

-- | What WB hands out: the execution log and the register write.
data WritebackOut = WriteBackOut
  { retired :: Maybe MaWb
  -- ^ The instruction that has just retired.
  , write :: Maybe (RegAddr, BitVector XLen)
  -- ^ The register write it asks for, absent when it writes none.
  }

-- | One clock of WB.
writeback :: WritebackIn -> WritebackOut
writeback WriteBackIn {entry} = WriteBackOut {retired = entry, write}
  where
    write = do
      maWb <- entry
      rdAddr <- destReg maWb
      Just (rdAddr, maWb.wbData)
