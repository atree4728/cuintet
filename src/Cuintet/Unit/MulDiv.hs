module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), mkMulDivInst, mulDivStep) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (DivOp (..), MulDivType (..), MulOp (..), Sign (..), XLen)
import Cuintet.Pipeline (IdEx (..))
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

data MulDivState = Idle | Busy {remaining :: Index (XLen + 1), acc :: BitVector (XLen * 2)}
  deriving (Generic, NFDataX)

-- | One clock of the unit.
mulDivStep :: MulDivState -> MulDivReq -> (MulDivState, MulDivResp)
mulDivStep _ MulDivReq {inst = Nothing} = (Idle, MulDivResp {stall = False, result = Nothing})
mulDivStep state MulDivReq {inst = Just inst, wready} = (state', MulDivResp {stall = isNothing result, result})
  where
    work = mkWork inst
    result = case state of
      Busy {remaining = 0, acc} -> Just $ finish work acc
      _ -> Nothing
    state'
      | isJust result = if wready then Idle else state
      | otherwise = case state of
          Idle -> Busy {remaining = maxBound, acc = zeroExtend work.streamed}
          Busy {remaining, acc} -> Busy {remaining = remaining - 1, acc = step work acc}

data Working = Working
  { mulDivType :: MulDivType
  , isOp32 :: Bool
  , neg1, neg2 :: Bool
  , held, streamed :: BitVector XLen
  }

mkWork :: MulDivInst -> Working
mkWork MulDivInst {..} = Working {..}
  where
    (sign1, sign2) = case mulDivType of
      Multiply MulLow -> (Unsigned, Unsigned)
      Multiply (MulHighHom sign) -> (sign, sign)
      Multiply MulHighHetero -> (Signed, Unsigned)
      Division (Div sign) -> (sign, sign)
      Division (Rem sign) -> (sign, sign)

    (held, streamed) = case mulDivType of
      Multiply _ -> (mag1, mag2) -- multiplicand, multiplier
      Division _ -> (mag2, mag1) -- divisor, dividend
    (neg1, mag1) = magnitude sign1 op1
    (neg2, mag2) = magnitude sign2 op2

    magnitude sign x = (neg, applyWhen neg negate narrowed)
      where
        narrowed = applyWhen isOp32 word x
        word y = case sign of
          Signed -> signExtend (truncateB y :: BitVector 32)
          Unsigned -> zeroExtend (truncateB y :: BitVector 32)
        neg = sign == Signed && msb narrowed == high

step :: Working -> BitVector (XLen * 2) -> BitVector (XLen * 2)
step Working {..} acc = case mulDivType of
  Multiply _ -> truncateB (added `shiftR` 1)
  Division _ -> acc
  where
    added :: BitVector (XLen * 2 + 1)
    added = applyWhen (lsb acc == high) (+ (zeroExtend held `shiftL` natToNum @XLen)) (zeroExtend acc)

finish :: Working -> BitVector (XLen * 2) -> BitVector XLen
finish Working {..} acc = applyWhen isOp32 word $ case mulDivType of
  Multiply MulLow -> lower
  Multiply _ -> upper
  Division _ -> 0
  where
    (upper, lower) = split $ applyWhen (neg1 /= neg2) negate acc
    word x = signExtend (truncateB x :: BitVector 32)
