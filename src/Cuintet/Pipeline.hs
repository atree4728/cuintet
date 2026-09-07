-- | The payloads that cross the stage boundaries, one record per FIFO.
module Cuintet.Pipeline (IfIdBits, IfId (..), IdEx (..), ExMa (..), MaCm (..), Retire (..), srcRegs, destReg, serializing, hasResult, srcAddrs) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isCsrRead, isLoad)
import Cuintet.Eei (Addr, Inst, MemReq, RegAddr, SystemOp (..), TrapCause, XLen)
import Cuintet.Unit.Btb (Prediction)
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)
import GHC.Records (HasField)

type IfIdBits = 3

data IfId = IfId
  { pc :: Addr
  , instBits :: Inst
  , prediction :: Maybe Prediction
  }
  deriving (Generic, NFDataX)

data IdEx = IdEx
  { pc :: Addr
  , instBits :: Inst
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  }
  deriving (Generic, NFDataX)

data ExMa = ExMa
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , rs1Addr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , rdAddr :: RegAddr
  , exception :: Maybe (TrapCause, BitVector XLen)
  , aluResult :: BitVector XLen
  , wbData :: BitVector XLen
  }
  deriving (Generic, NFDataX)

data MaCm = MaCm
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , rs1Addr :: RegAddr
  , rs1Data :: BitVector XLen
  , rdAddr :: RegAddr
  , exception :: Maybe (TrapCause, BitVector XLen)
  , wbData :: BitVector XLen
  , completed :: Maybe MemReq
  }
  deriving (Generic, NFDataX)

data Retire = Retire
  { pc :: Addr
  , instBits :: Inst
  , rd :: Maybe (RegAddr, BitVector XLen)
  , mem :: Maybe MemReq
  , trap :: Maybe TrapCause
  }
  deriving (Generic, NFDataX, Eq)

-- | The @rs1@ and @rs2@ fields, shared by ID and the register file read.
srcRegs :: Inst -> (RegAddr, RegAddr)
srcRegs instBits = (unpack $ slice d19 d15 instBits, unpack $ slice d24 d20 instBits)

srcAddrs :: Maybe IfId -> Vec 2 RegAddr
srcAddrs entry = rs1Addr :> rs2Addr :> Nil
  where
    (rs1Addr, rs2Addr) = maybe (0, 0) (srcRegs . (.instBits)) entry

-- | The register this instruction writes
destReg ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
destReg stage = orNothing (isNothing stage.exception && stage.ctrl.rwbEn && stage.rdAddr /= 0) stage.rdAddr

hasResult :: (HasField "ctrl" stage InstCtrl) => stage -> Bool
hasResult stage = not (isLoad stage.ctrl || isCsrRead stage.ctrl)

serializing :: (HasField "exception" stage (Maybe a), HasField "ctrl" stage InstCtrl) => stage -> Bool
serializing stage = isJust stage.exception || stage.ctrl.systemOp == Just SysMret
