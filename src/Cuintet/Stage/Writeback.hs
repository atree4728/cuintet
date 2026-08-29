-- | WB: turns an instruction that has committed into a register write, and hands the entry out as the core's execution log.
module Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback) where

import Clash.Prelude
import Cuintet.Pipeline (MaWb (..), Retire (..), forwardable)

-- | The instruction leaving the MA-WB FIFO. Its presence means it committed.
newtype WritebackIn = WriteBackIn {entry :: Maybe MaWb}

-- | What WB hands out: the execution log and the register write.
newtype WritebackOut = WriteBackOut {retired :: Maybe Retire}

retire :: MaWb -> Retire
retire stage =
  Retire
    { pc = stage.pc
    , instBits = stage.instBits
    , rd = forwardable stage
    , mem = stage.completed
    , trap = fst <$> stage.exception
    }

-- | One clock of WB.
writeback :: WritebackIn -> WritebackOut
writeback WriteBackIn {entry} = WriteBackOut {retired = retire <$> entry}
{-# OPAQUE writeback #-}
