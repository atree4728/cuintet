module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), mkMulDivInst, mulDivStep) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (DivOp (..), MulDivType (..), MulOp (..), Sign (..), XLen)
import Cuintet.Pipeline (IdEx (..))
import Cuintet.Unit.MulDiv.Div (DivOperands (..), DivResult (..), DivState, divInit, divResult, divStep)
import Cuintet.Unit.MulDiv.Mul (MulOperands (..), MulState, mulInit, mulProduct, mulStep)
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
    mags@(mag1, mag2) = magnitudes inst
    mulOps = MulOperands {multiplicand = mag1.value, multiplier = mag2.value}
    divOps = DivOperands {dividend = mag1.value, divisor = mag2.value}

    result = case (state, inst.mulDivType) of
      (Multiplying st, Multiply op) -> reduceMul inst.isOp32 op mags <$> mulProduct st
      (Dividing st, Division op) -> reduceDiv inst.isOp32 op mags <$> divResult st
      _ -> Nothing

    state'
      | isJust result = if wready then Idle else state
      | otherwise = case inst.mulDivType of
          Multiply _ -> Multiplying $ case state of
            Multiplying st -> mulStep mulOps st
            _ -> mulInit mulOps
          Division _ -> Dividing $ case state of
            Dividing st -> divStep divOps st
            _ -> divInit divOps

data Magnitude = Magnitude {negative :: Bool, value :: Unsigned XLen}

magnitudes :: MulDivInst -> (Magnitude, Magnitude)
magnitudes MulDivInst {..} = (magnitude sign1 op1, magnitude sign2 op2)
  where
    (sign1, sign2) = case mulDivType of
      Multiply MulLow -> (Unsigned, Unsigned)
      Multiply (MulHighHom sign) -> (sign, sign)
      Multiply MulHighHetero -> (Signed, Unsigned)
      Division (Div sign) -> (sign, sign)
      Division (Rem sign) -> (sign, sign)

    magnitude sign x = Magnitude {negative, value = applyWhen negative negate (bitCoerce narrowed)}
      where
        narrowed = applyWhen isOp32 word x
        word y = case sign of
          Signed -> signExtend (truncateB y :: BitVector 32)
          Unsigned -> zeroExtend (truncateB y :: BitVector 32)
        negative = sign == Signed && msb narrowed == high

reduceMul :: Bool -> MulOp -> (Magnitude, Magnitude) -> Unsigned (XLen * 2) -> BitVector XLen
reduceMul isOp32 op (a, b) prod = sextWord isOp32 $ case op of
  MulLow -> lower
  _ -> upper
  where
    (upper, lower) = split $ pack $ applyWhen (a.negative /= b.negative) negate prod

reduceDiv :: Bool -> DivOp -> (Magnitude, Magnitude) -> DivResult -> BitVector XLen
reduceDiv isOp32 op (a, b) DivResult {..} = sextWord isOp32 $ case op of
  Div _ | b.value == 0 -> maxBound
  Div _ -> pack $ applyWhen (a.negative /= b.negative) negate quotient
  Rem _ -> pack $ applyWhen a.negative negate remainder

sextWord :: Bool -> BitVector XLen -> BitVector XLen
sextWord isOp32 = applyWhen isOp32 (\x -> signExtend (truncateB x :: BitVector 32))
