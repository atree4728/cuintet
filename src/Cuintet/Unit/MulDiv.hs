module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), mkMulDivInst, mulDivStep) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (DivOp (..), MulDivType (..), MulOp (..), Sign (..), XLen)
import Cuintet.Pipeline (IdEx (..))
import Cuintet.Unit.MulDiv.Div (DivOperands (..), DivResult (..), DivState, divInit, divResult, divStep)
import Cuintet.Unit.MulDiv.Mul (MulOperands (..), MulResult (..), MulState, mulInit, mulResult, mulStep)
import Data.Function (applyWhen)
import Data.Maybe (isJust, isNothing)

data MulDivInst = MulDivInst
  { mulDivType :: MulDivType
  , isOp32 :: Bool
  , op1, op2 :: BitVector XLen
  }

mkMulDivInst :: IdEx -> Maybe MulDivInst
mkMulDivInst IdEx {..}
  | Just mulDivType <- ctrl.mulDiv = Just MulDivInst {mulDivType, isOp32 = ctrl.isOp32, op1 = rs1Data, op2 = rs2Data}
  | otherwise = Nothing

data MulDivReq = MulDivReq
  { inst :: Maybe MulDivInst
  , wready :: Bool
  }

data MulDivResp = MulDivResp
  { stall :: Bool
  , result :: Maybe (BitVector XLen)
  }

data MulDivState = Idle | Multiplying MulState | Dividing DivState
  deriving (Generic, NFDataX)

mulDivStep :: MulDivState -> MulDivReq -> (MulDivState, MulDivResp)
mulDivStep _ MulDivReq {inst = Nothing} = (Idle, MulDivResp {stall = False, result = Nothing})
mulDivStep state MulDivReq {inst = Just inst, wready} = (state', MulDivResp {stall = isNothing result, result})
  where
    (result, stepped) = case inst.mulDivType of
      Multiply op -> (finish <$> (mulResult =<< running), Multiplying next)
        where
          running = case state of
            Multiplying st -> Just st
            _ -> Nothing
          (signs, pick) = case op of
            MulLow -> ((Signed, Signed), snd)
            MulHighHom sign -> ((sign, sign), fst)
            MulHighHetero -> ((Signed, Unsigned), fst)
          ops = mulOperands signs inst
          next = maybe (mulInit ops) (mulStep ops) running
          finish mres = sextWord inst.isOp32 $ pick (bitCoerce mres.product)
      Division op -> (finish <$> (divResult =<< running), Dividing next)
        where
          running = case state of
            Dividing st -> Just st
            _ -> Nothing
          (sign, pick) = case op of
            Div s -> (s, fst)
            Rem s -> (s, snd)
          (dividend, divisor) = magnitudes sign inst
          ops = DivOperands {dividend = dividend.value, divisor = divisor.value}
          next = maybe (divInit ops) (divStep ops) running
          finish DivResult {quotient = q, remainder = r} = sextWord inst.isOp32 . pack $ pick (quotient, remainder)
            where
              quotient
                | divisor.value == 0 = maxBound
                | otherwise = applyWhen (dividend.negative /= divisor.negative) negate q
              remainder = applyWhen dividend.negative negate r

    state'
      | isJust result = if wready then Idle else state
      | otherwise = stepped

mulOperands :: (Sign, Sign) -> MulDivInst -> MulOperands
mulOperands (sign1, sign2) MulDivInst {op1, op2} =
  MulOperands {multiplicand = widen sign1 op1, multiplier = widen sign2 op2}
  where
    widen sign =
      unpack . case sign of
        Signed -> signExtend
        Unsigned -> zeroExtend

data Magnitude = Magnitude {negative :: Bool, value :: Unsigned XLen}

magnitudes :: Sign -> MulDivInst -> (Magnitude, Magnitude)
magnitudes sign MulDivInst {isOp32, op1, op2} = (magnitude op1, magnitude op2)
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
