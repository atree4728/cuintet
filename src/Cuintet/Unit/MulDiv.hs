module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), MulDivJob (..), mulDivStep) where

import Clash.Prelude
import Cuintet.Completion (Completion (..), regWrite)
import Cuintet.Eei (DivOp (..), MulDivOp (..), MulOp (..), PRegAddr, RobAddr, Sign (..), XLen)
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
  Complete robAddr pdAddr RobDone {exception = Nothing, mispredicted, value, mem = Nothing}

mulDivStep :: MulDivState -> MulDivReq -> (MulDivState, MulDivResp)
mulDivStep _ MulDivReq {squash = True} = (Idle, MulDivResp {busy = False, done = Nothing, bypass = Nothing})
mulDivStep Idle MulDivReq {job} =
  (maybe Idle (`Busy` Loaded) job, MulDivResp {busy = False, done = Nothing, bypass = Nothing})
mulDivStep (Waiting c) MulDivReq {granted} =
  (if granted then Idle else Waiting c, MulDivResp {busy = True, done = Just c, bypass = regWrite c})
mulDivStep (Busy job phase) MulDivReq {granted} = (state', MulDivResp {busy = True, done, bypass})
  where
    done = completion job <$> result
    bypass = (,) <$> job.pdAddr <*> result

    state' = case result of
      Just v
        | granted -> Idle
        | otherwise -> Waiting (completion job v)
      Nothing -> Busy job stepped

    (result, stepped) = case job.mulDivOp of
      Multiply op -> (finish <$> (mulResult =<< running), Multiplying next)
        where
          running = case phase of
            Multiplying st -> Just st
            _ -> Nothing
          (signs, pick) = case op of
            MulLow -> ((Signed, Signed), snd)
            MulHighHom sign -> ((sign, sign), fst)
            MulHighHetero -> ((Signed, Unsigned), fst)
          ops = mulOperands signs job
          next = maybe (mulInit ops) (mulStep ops) running
          finish mres = sextWord job.isOp32 $ pick (bitCoerce mres.product)
      Division op -> (finish <$> (divResult =<< running), Dividing next)
        where
          running = case phase of
            Dividing st -> Just st
            _ -> Nothing
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
{-# OPAQUE mulDivStep #-}

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
