-- | WB: the write ports. Each ALU port has its own, and so does the store port, which writes the ROB alone; the units share the last, the load first.
module Cuintet.Stage.WriteBack (writeback, WriteBackIn (..), WriteBackOut (..)) where

import Clash.Prelude
import Cuintet.Completion (Completion (..), regWrite, robWrite)
import Cuintet.Eei (NAluPorts, PRegAddr, RobAddr, WriteBackWidth, XLen)
import Cuintet.Pipeline (Executed (..))
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Util ((<<$>>))
import Data.Maybe (isNothing)

data WriteBackIn = WriteBackIn
  { aluExecuted :: Vec NAluPorts (Maybe Executed)
  , storeExecuted :: Maybe Executed
  , mulDivDone :: Maybe Completion
  , loadDone :: Maybe Completion
  }

data WriteBackOut = WriteBackOut
  { regWrites :: Vec WriteBackWidth (Maybe (PRegAddr, BitVector XLen))
  , robWrites :: Vec (WriteBackWidth + 1) (Maybe (RobAddr, RobDone))
  , mulDivGranted :: Bool
  , loadGranted :: Bool
  }

writeback :: WriteBackIn -> WriteBackOut
writeback WriteBackIn {..} = WriteBackOut {..}
  where
    completions = (completed <<$>> aluExecuted) :< (loadDone <|> mulDivDone)
    loadGranted = True
    mulDivGranted = isNothing loadDone

    regWrites = (regWrite =<<) <$> completions
    robWrites = (robWrite <<$>> completions) :< (robWrite . completed <$> storeExecuted)

    completed entry@Executed {..} =
      Completion entry.robAddr pdAddr RobDone {exception, mispredicted, value = entry.wbData, mem}
{-# OPAQUE writeback #-}
