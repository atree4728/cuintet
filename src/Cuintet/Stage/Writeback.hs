-- | WB: turns an instruction that has committed into a register write, and hands the entry out as the core's execution log.
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
{-# OPAQUE writeback #-}
