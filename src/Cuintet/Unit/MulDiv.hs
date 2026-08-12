module Cuintet.Unit.MulDiv (MulDivReq (..), MulDivResp (..), MulDivState (..), mkMulDivInst, mulDivStep) where

import Clash.Prelude
import Control.Arrow (left)
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (MulDivType, XLen)
import Cuintet.Pipeline (IdEx (..))
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

data Work = Multiplying () | Dividing ()
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

start :: MulDivInst -> Either Work (BitVector XLen)
start = undefined

step :: MulDivInst -> Work -> Either Work (BitVector XLen)
step = undefined
