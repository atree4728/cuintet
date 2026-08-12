module Cuintet.Unit.MulDiv.Mul (MulOperands (..), MulState, mulInit, mulStep, mulResult) where

import Clash.Prelude
import Cuintet.Eei (XLen)

data MulOperands = MulOperands {multiplicand, multiplier :: Signed (XLen + 1)}

-- | Radix-4 Booth groups needed to recode a multiplier of @XLen + 2@ bits.
type Groups = XLen `Div` 2 + 1

data MulState
  = Running
      { remaining :: Index Groups
      , mplier :: BitVector (XLen + 2)
      , acc :: Signed (XLen * 2 + 2)
      }
  | Done (BitVector (XLen * 2))
  deriving (Generic, NFDataX)

mulInit :: MulOperands -> MulState
mulInit MulOperands {multiplier} =
  Running
    { remaining = maxBound
    , mplier = pack (signExtend multiplier :: Signed (XLen + 2))
    , acc = 0
    }

mulStep :: MulOperands -> MulState -> MulState
mulStep MulOperands {multiplicand} = \case
  Running {remaining = 0, mplier, acc} -> Done (truncateB (pack (advance mplier acc)))
  Running {..} -> Running {remaining = remaining - 1, mplier = mplier `shiftL` 2, acc = advance mplier acc}
  Done {} -> deepErrorX "mul: stepped after completion"
  where
    advance mplier acc = (acc `shiftL` 2) + multiple (truncateB $ mplier `shiftR` natToNum @(XLen - 1))

    multiple :: BitVector 3 -> Signed (XLen * 2 + 2)
    multiple = \case
      0b001 -> m
      0b010 -> m
      0b011 -> m `shiftL` 1
      0b100 -> negate (m `shiftL` 1)
      0b101 -> negate m
      0b110 -> negate m
      _ -> 0
      where
        m = extend multiplicand

mulResult :: MulState -> Maybe (BitVector (XLen * 2))
mulResult = \case
  Done prod -> Just prod
  Running {} -> Nothing
