-- | WB: the write ports. Each ALU port has its own, and so does the store port, which writes the ROB alone; the units and Cm share the last, Cm first, then the load, then the multiply\/divide.
module Cuintet.Stage.WriteBack (writeback, WriteBackIn (..), WriteBackOut (..)) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Completion (Completion (..), regWrite, robWrite)
import Cuintet.CoreCtrl (Wakeup (..), opClassOf, wakeup)
import Cuintet.Eei (NAluPorts, PRegAddr, RobAddr, WriteBackWidth, XLen)
import Cuintet.Pipeline (Executed (..))
import Cuintet.Unit.Rob (RobDone (..))
import Data.Maybe (isNothing)

data WriteBackIn = WriteBackIn
  { alus :: Vec NAluPorts (Maybe Executed)
  , store :: Maybe Executed
  , mulDivDone :: Maybe Completion
  , loadDone :: Maybe Completion
  , csrWrite :: Maybe (PRegAddr, BitVector XLen)
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
    completions = (fmap completed <$> alus) :< (uncurry CsrValue <$> csrWrite <|> loadDone <|> mulDivDone)
    loadGranted = isNothing csrWrite
    mulDivGranted = isNothing csrWrite && isNothing loadDone

    regWrites = (regWrite =<<) <$> completions
    robWrites = ((robWrite =<<) <$> completions) :< (robWrite . completed =<< store)

    completed entry@Executed {..} =
      Complete entry.robAddr pd RobDone {exception, mispredicted, value = entry.wbData, mem}
      where
        pd = guard (wakeup (opClassOf ctrl) /= AtCommit) *> pdAddr
{-# OPAQUE writeback #-}
