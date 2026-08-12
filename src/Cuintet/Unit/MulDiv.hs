module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), mkMulDivInst, mulDivStep) where

import Clash.Prelude
import Control.Arrow (left)
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (MulDivType (..), MulOp (..), Sign (..), XLen)
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

data Work
  = Multiplying
      { counter :: Index (XLen + 1)
      , result, op1zext, op2zext :: BitVector (XLen * 2)
      }
  | Dividing {}
  deriving (Generic, NFDataX)

data MulDivState = Idle | Busy Work | Done (BitVector XLen)
  deriving (Generic, NFDataX)

mulDivStep :: MulDivState -> MulDivReq -> (MulDivState, MulDivResp)
mulDivStep state MulDivReq {..} = (state', MulDivResp {..})
  where
    progress = case (state, inst) of
      (Idle, Just i) -> left Busy $ start i
      (Busy w, Just i) -> left Busy $ step i w
      (Done r, _) -> Right r
      _ -> Left Idle
    result = either (const Nothing) Just progress
    stall = isJust inst && isNothing result
    state'
      | isNothing inst = Idle
      | Right r <- progress = if wready then Idle else Done r
      | Left s <- progress = s

mulOperands :: MulOp -> BitVector XLen -> BitVector XLen -> (Bool, BitVector (XLen * 2), BitVector (XLen * 2))
mulOperands mulOp op1 op2 = (neg1 /= neg2, magnitude neg1 op1, magnitude neg2 op2)
  where
    (neg1, neg2) = case mulOp of
      MulLow -> (False, False)
      MulHighHom Unsigned -> (False, False)
      MulHighHom Signed -> (isNegative op1, isNegative op2)
      MulHighHetero -> (isNegative op1, False)
    isNegative x = msb x == high
    magnitude neg = zeroExtend . applyWhen neg negate

start :: MulDivInst -> Either Work (BitVector XLen)
start MulDivInst {..}
  | Multiply mulOp <- mulDivType =
      let (_, op1zext, op2zext) = mulOperands mulOp op1 op2
       in Left $ Multiplying {counter = 0, result = 0, op1zext, op2zext}
  | Division _ <- mulDivType = Right 0

step :: MulDivInst -> Work -> Either Work (BitVector XLen)
step MulDivInst {..} Multiplying {..}
  | counter == natToNum @XLen = Right $ finish mulDivType
  | otherwise =
      Left $
        Multiplying
          { counter = counter + 1
          , result = applyWhen (op2zext `testBit` fromIntegral counter) (+ op1zext) result
          , op1zext = op1zext `shiftL` 1
          , op2zext
          }
  where
    -- the half of the signed product the instruction asks for; @MULW@ keeps the low word only
    finish (Multiply mulOp) = applyWhen isOp32 word $ if mulOp == MulLow then lower else upper
      where
        (neg, _, _) = mulOperands mulOp op1 op2
        (upper, lower) = split $ applyWhen neg negate result
    finish _ = deepErrorX "mulDiv: division is not implemented"

    word x = signExtend (truncateB x :: BitVector 32)
step _ Dividing = Right 0
