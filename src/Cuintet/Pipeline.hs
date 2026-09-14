-- | The payloads that cross the stage boundaries, one record per FIFO.
module Cuintet.Pipeline (FetchBufBits, Fetched (..), Decoded (..), Renamed (..), Ready (..), Executed (..), Retire (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, Inst, MemReq, PRegAddr, RegAddr, RobAddr, TrapCause, XLen)
import Cuintet.Unit.Btb (Prediction)

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
  , rdAddr :: Maybe RegAddr
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
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: Maybe PRegAddr
  -- ^ 'Nothing' when the instruction traps.
  , robAddr :: RobAddr
  , mispredicted :: Bool
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
