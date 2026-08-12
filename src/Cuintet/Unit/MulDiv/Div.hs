module Cuintet.Unit.MulDiv.Div (DivOperands (..), DivResult (..), DivState, divInit, divStep, divResult) where

import Clash.Prelude
import Cuintet.Eei (XLen)
import Data.Function (applyWhen)

data DivOperands = DivOperands {dividend, divisor :: Unsigned XLen}

data DivResult = DivResult {quotient, remainder :: Unsigned XLen}
  deriving (Generic, NFDataX)

data DivState
  = Running {remaining :: Index XLen, acc :: Unsigned (XLen * 2)}
  | Done DivResult
  deriving (Generic, NFDataX)

divInit :: DivOperands -> DivState
divInit DivOperands {dividend} = Running {remaining = maxBound, acc = extend dividend}

divStep :: DivOperands -> DivState -> DivState
divStep DivOperands {divisor} = \case
  Running {remaining = 0, acc} -> Done (toResult (advance acc))
  Running {remaining, acc} -> Running {remaining = remaining - 1, acc = advance acc}
  Done {} -> deepErrorX "div: stepped after completion"
  where
    upperDivisor :: Unsigned (XLen * 2 + 1)
    upperDivisor = extend divisor `shiftL` natToNum @XLen

    advance :: Unsigned (XLen * 2) -> Unsigned (XLen * 2)
    advance acc = truncateB $ applyWhen fits (\x -> x - upperDivisor + 1) shifted
      where
        -- the shift leaves the low bit clear, so setting the quotient bit is just +1
        shifted = extend acc `shiftL` 1 :: Unsigned (XLen * 2 + 1)
        fits = (truncateB (shifted `shiftR` natToNum @XLen) :: Unsigned (XLen + 1)) >= extend divisor

    toResult acc = DivResult {quotient, remainder}
      where
        (remainder, quotient) = bitCoerce acc

divResult :: DivState -> Maybe DivResult
divResult = \case
  Done result -> Just result
  Running {} -> Nothing
