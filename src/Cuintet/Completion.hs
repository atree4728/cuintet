-- | What one write port of WB carries: the ROB completion and the physical register write.
module Cuintet.Completion (Completion (..), robWrite, regWrite) where

import Clash.Prelude
import Cuintet.Eei (PRegAddr, RobAddr, XLen)
import Cuintet.Unit.Rob (RobDone (..))

data Completion
  = Complete RobAddr (Maybe PRegAddr) RobDone
  | CsrValue PRegAddr (BitVector XLen)
  deriving (Generic, NFDataX)

robWrite :: Completion -> Maybe (RobAddr, RobDone)
robWrite = \case
  Complete robAddr _ done -> Just (robAddr, done)
  CsrValue {} -> Nothing

regWrite :: Completion -> Maybe (PRegAddr, BitVector XLen)
regWrite = \case
  Complete _ pdAddr done -> (,done.value) <$> pdAddr
  CsrValue pdAddr value -> Just (pdAddr, value)
