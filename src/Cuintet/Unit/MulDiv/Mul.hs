module Cuintet.Unit.MulDiv.Mul (MulOperands (..), MulState, mulInit, mulStep, mulProduct) where

import Clash.Prelude
import Cuintet.Eei (XLen)
import Data.Function (applyWhen)

data MulOperands = MulOperands {multiplicand, multiplier :: Unsigned XLen}

data MulState
  = Running {remaining :: Index XLen, acc :: Unsigned (XLen * 2)}
  | Done (Unsigned (XLen * 2))
  deriving (Generic, NFDataX)

mulInit :: MulOperands -> MulState
mulInit MulOperands {..} = Running {remaining = maxBound, acc = extend multiplier}

mulStep :: MulOperands -> MulState -> MulState
mulStep MulOperands {..} = \case
  Running {remaining = 0, acc} -> Done (advance acc)
  Running {remaining, acc} -> Running {remaining = remaining - 1, acc = advance acc}
  Done {} -> deepErrorX "mul: stepped after completion"
  where
    addend :: Unsigned (XLen * 2 + 1)
    addend = extend multiplicand `shiftL` natToNum @XLen

    advance :: Unsigned (XLen * 2) -> Unsigned (XLen * 2)
    advance acc = truncateB $ applyWhen (acc `testBit` 0) (+ addend) (extend acc) `shiftR` 1

mulProduct :: MulState -> Maybe (Unsigned (XLen * 2))
mulProduct = \case
  Done prod -> Just prod
  Running {} -> Nothing
