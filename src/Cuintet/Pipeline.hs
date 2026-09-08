-- | The payloads that cross the stage boundaries, one record per FIFO.
module Cuintet.Pipeline (FetchBufBits, Fetched (..), Decoded (..), Renamed (..), Ready (..), Executed (..), Completed (..), Retire (..), rdOf, pdOf, serializing, hasResult) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isCsrRead, isLoad)
import Cuintet.Eei (Addr, Inst, MemReq, PRegAddr, RegAddr, SystemOp (..), TrapCause, XLen)
import Cuintet.Unit.Btb (Prediction)
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
  , instBits :: Inst
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , exception :: Maybe (TrapCause, BitVector XLen)
  , ps1Addr :: PRegAddr
  , ps2Addr :: PRegAddr
  , pdAddr :: PRegAddr
  , oldPdAddr :: PRegAddr
  }
  deriving (Generic, NFDataX)

data Ready = Ready
  { pc :: Addr
  , instBits :: Inst
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: PRegAddr
  , oldPdAddr :: PRegAddr
  }
  deriving (Generic, NFDataX)

data Executed = Executed
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , rs1Addr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , rdAddr :: RegAddr
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: PRegAddr
  , oldPdAddr :: PRegAddr
  , aluResult :: BitVector XLen
  , wbData :: BitVector XLen
  }
  deriving (Generic, NFDataX)

data Completed = Completed
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , rs1Addr :: RegAddr
  , rs1Data :: BitVector XLen
  , rdAddr :: RegAddr
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: PRegAddr
  , oldPdAddr :: PRegAddr
  , wbData :: BitVector XLen
  , mem :: Maybe MemReq
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

rdOf ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
rdOf stage = orNothing (isNothing stage.exception && stage.ctrl.rwbEn && stage.rdAddr /= 0) stage.rdAddr

pdOf ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  , HasField "pdAddr" stage PRegAddr
  ) =>
  stage -> Maybe PRegAddr
pdOf stage = stage.pdAddr <$ rdOf stage

hasResult :: (HasField "ctrl" stage InstCtrl) => stage -> Bool
hasResult stage = not (isLoad stage.ctrl || isCsrRead stage.ctrl)

serializing :: (HasField "exception" stage (Maybe a), HasField "ctrl" stage InstCtrl) => stage -> Bool
serializing stage = isJust stage.exception || stage.ctrl.systemOp == Just SysMret
