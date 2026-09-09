-- | The payloads that cross the stage boundaries, one record per FIFO.
module Cuintet.Pipeline (FetchBufBits, Fetched (..), Decoded (..), Renamed (..), Ready (..), Executed (..), Retire (..), Completion (..), validRdOf, rdOf, pdOf, isSerializing, hasResult, regWrite, robWrite, usesMulDiv) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (ExecUnit (..), InstCtrl (..), isCsrRead, unitOf)
import Cuintet.Eei (Addr, Inst, MemReq, PRegAddr, RegAddr, RobAddr, SystemOp (..), TrapCause, XLen)
import Cuintet.Unit.Btb (Prediction)
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)
import GHC.Records (HasField)

type FetchBufBits = 3

data Fetched = Fetched
  { pc :: Addr
  , instBits :: Inst
  , prediction :: Maybe Prediction
  }
  deriving (Generic, NFDataX)

data Decoded = Decoded
  { pc :: Addr
  , instBits :: Inst
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , exception :: Maybe (TrapCause, BitVector XLen)
  }
  deriving (Generic, NFDataX)

data Renamed = Renamed
  { pc :: Addr
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  , ps1Addr :: PRegAddr
  , ps2Addr :: PRegAddr
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  }
  deriving (Generic, NFDataX)

data Ready = Ready
  { pc :: Addr
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  }
  deriving (Generic, NFDataX)

data Executed = Executed
  { ctrl :: InstCtrl
  , rs2Data :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  , mispredicted :: Bool
  , aluResult :: BitVector XLen
  , wbData :: BitVector XLen
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

data Completion
  = Complete RobAddr (Maybe PRegAddr) RobDone
  | CsrValue PRegAddr (BitVector XLen)
  deriving (Generic, NFDataX)

validRdOf ::
  ( HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
validRdOf stage = orNothing (stage.ctrl.rwbEn && stage.rdAddr /= 0) stage.rdAddr

rdOf ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
rdOf stage = guard (isNothing stage.exception) *> validRdOf stage

pdOf ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "pdAddr" stage (Maybe PRegAddr)
  ) =>
  stage -> Maybe PRegAddr
pdOf stage = guard (isNothing stage.exception) *> stage.pdAddr

hasResult :: (HasField "ctrl" stage InstCtrl) => stage -> Bool
hasResult stage = case unitOf stage.ctrl of
  Alu _ -> not (isCsrRead stage.ctrl)
  _ -> False

isSerializing :: (HasField "exception" stage (Maybe a), HasField "ctrl" stage InstCtrl) => stage -> Bool
isSerializing stage = isJust stage.exception || stage.ctrl.systemOp == Just SysMret

usesMulDiv :: (HasField "exception" stage (Maybe a), HasField "ctrl" stage InstCtrl) => stage -> Bool
usesMulDiv entry = isNothing entry.exception && unitOf entry.ctrl == MulDiv

robWrite :: Completion -> Maybe (RobAddr, RobDone)
robWrite = \case
  Complete robAddr _ done -> Just (robAddr, done)
  CsrValue {} -> Nothing

regWrite :: Completion -> Maybe (PRegAddr, BitVector XLen)
regWrite = \case
  Complete _ pdAddr done -> (,done.value) <$> pdAddr
  CsrValue pdAddr value -> Just (pdAddr, value)
