-- | What one write port of WB carries: the ROB completion and the physical register write.
module Cuintet.Completion (Completion (..), robWrite, regWrite, trapped) where

import Clash.Prelude
import Cuintet.Eei (PRegAddr, RobAddr, TrapCause, XLen)
import Cuintet.Unit.Rob (RobDone (..))

data Completion = Completion RobAddr (Maybe PRegAddr) RobDone
  deriving (Generic, NFDataX)

robWrite :: Completion -> (RobAddr, RobDone)
robWrite (Completion robAddr _ done) = (robAddr, done)

regWrite :: Completion -> Maybe (PRegAddr, BitVector XLen)
regWrite (Completion _ pdAddr done) = (,done.value) <$> pdAddr

trapped :: RobAddr -> (TrapCause, BitVector XLen) -> Completion
trapped robAddr exception = Completion robAddr Nothing RobDone {exception = Just exception, mispredicted = False, value = 0, mem = Nothing}
