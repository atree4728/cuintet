module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), MulDivJob (..), mulDivStep, holder) where

import Clash.Prelude
import Cuintet.Completion (Completion (..), regWrite, robWrite, trapped)
import Cuintet.Eei (DivOp (..), MulDivOp (..), MulOp (..), PRegAddr, RobAddr, Sign (..), TrapCause, XLen)
import Cuintet.Unit.MulDiv.Div (DivOperands (..), DivResult (..), DivState, divInit, divResult, divStep)
import Cuintet.Unit.MulDiv.Mul (MulOperands (..), MulResult (..), MulState, mulInit, mulResult, mulStep)
import Cuintet.Unit.Rob (RobDone (..))
import Data.Function (applyWhen)

-- | One multiply or divide for the unit to carry out.
data MulDivJob = MulDivJob
  { mulDivOp :: MulDivOp
  , isOp32 :: Bool
  , op1, op2 :: BitVector XLen
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  , mispredicted :: Bool
  , exception :: Maybe (TrapCause, BitVector XLen)
  }
  deriving (Generic, NFDataX)

data MulDivReq = MulDivReq
  { job :: Maybe MulDivJob
  , granted :: Bool
  , squash :: Bool
  }

data MulDivResp = MulDivResp
  { busy :: Bool
  , done :: Maybe Completion
  , bypass :: Maybe (PRegAddr, BitVector XLen)
  }

data MulDivState = Idle | Busy MulDivJob Phase | Waiting Completion
  deriving (Generic, NFDataX)

data Phase = Loaded | Multiplying MulState | Dividing DivState
  deriving (Generic, NFDataX)

completion :: MulDivJob -> BitVector XLen -> Completion
completion MulDivJob {robAddr, pdAddr, mispredicted} value =
  Completion robAddr pdAddr RobDone {exception = Nothing, mispredicted, value, mem = Nothing}

mulDivStep :: MulDivState -> MulDivReq -> (MulDivState, MulDivResp)
mulDivStep state MulDivReq {..} = (state', MulDivResp {busy, done, bypass = regWrite =<< done})
  where
    (state', done)
      | squash = (Idle, Nothing)
      | otherwise = case state of
          Idle -> (maybe Idle start job, Nothing)
          Busy j phase -> case advance j phase of
            Left phase' -> (Busy j phase', Nothing)
            Right value -> settle (completion j value)
          Waiting c -> settle c

    start j = maybe (Busy j Loaded) (Waiting . trapped j.robAddr) j.exception

    settle c = (if granted then Idle else Waiting c, Just c)

    busy =
      not squash && case state of
        Idle -> False
        _ -> True
{-# OPAQUE mulDivStep #-}

-- | The result, once the phase holds it; the next phase until then.
advance :: MulDivJob -> Phase -> Either Phase (BitVector XLen)
advance job phase = case job.mulDivOp of
  Multiply op -> advanceMul op job $ case phase of
    Multiplying st -> Just st
    _ -> Nothing
  Division op -> advanceDiv op job $ case phase of
    Dividing st -> Just st
    _ -> Nothing

advanceMul :: MulOp -> MulDivJob -> Maybe MulState -> Either Phase (BitVector XLen)
advanceMul op job running = maybe (Left (Multiplying next)) (Right . finish) (mulResult =<< running)
  where
    (signs, pick) = case op of
      MulLow -> ((Signed, Signed), snd)
      MulHighHom sign -> ((sign, sign), fst)
      MulHighHetero -> ((Signed, Unsigned), fst)
    ops = mulOperands signs job
    next = maybe (mulInit ops) (mulStep ops) running
    finish mres = sextWord job.isOp32 $ pick (bitCoerce mres.product)

advanceDiv :: DivOp -> MulDivJob -> Maybe DivState -> Either Phase (BitVector XLen)
advanceDiv op job running = maybe (Left (Dividing next)) (Right . finish) (divResult =<< running)
  where
    (sign, pick) = case op of
      Div s -> (s, fst)
      Rem s -> (s, snd)
    (dividend, divisor) = magnitudes sign job
    ops = DivOperands {dividend = dividend.value, divisor = divisor.value}
    next = maybe (divInit ops) (divStep ops) running
    finish DivResult {quotient = q, remainder = r} = sextWord job.isOp32 . pack $ pick (quotient, remainder)
      where
        quotient
          | divisor.value == 0 = maxBound
          | otherwise = applyWhen (dividend.negative /= divisor.negative) negate q
        remainder = applyWhen dividend.negative negate r

-- | The entry the unit holds, for the trace.
holder :: MulDivState -> Maybe RobAddr
holder = \case
  Busy job _ -> Just job.robAddr
  Waiting c -> Just (fst (robWrite c))
  Idle -> Nothing

mulOperands :: (Sign, Sign) -> MulDivJob -> MulOperands
mulOperands (sign1, sign2) MulDivJob {op1, op2} =
  MulOperands {multiplicand = widen sign1 op1, multiplier = widen sign2 op2}
  where
    widen sign =
      unpack . case sign of
        Signed -> signExtend
        Unsigned -> zeroExtend

data Magnitude = Magnitude {negative :: Bool, value :: Unsigned XLen}

magnitudes :: Sign -> MulDivJob -> (Magnitude, Magnitude)
magnitudes sign MulDivJob {isOp32, op1, op2} = (magnitude op1, magnitude op2)
  where
    magnitude x = Magnitude {negative, value = applyWhen negative negate (bitCoerce narrowed)}
      where
        narrowed = applyWhen isOp32 word x
        word y = case sign of
          Signed -> signExtend (truncateB y :: BitVector 32)
          Unsigned -> zeroExtend (truncateB y :: BitVector 32)
        negative = sign == Signed && msb narrowed == high

sextWord :: Bool -> BitVector XLen -> BitVector XLen
sextWord isOp32 = applyWhen isOp32 (\x -> signExtend (truncateB x :: BitVector 32))
