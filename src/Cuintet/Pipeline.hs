-- | The payloads that cross the stage boundaries, one record per FIFO.
module Cuintet.Pipeline (IfId (..), IdEx (..), ExMa (..), MaWb (..), Retire (..), srcRegs, destReg, forwardable, unresolved) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isCsrRead, isLoad)
import Cuintet.Eei (Addr, Inst, MemReq, RegAddr, TrapCause, XLen)
import Cuintet.Unit.Btb (Prediction)
import Cuintet.Util (orNothing)
import Data.Maybe (isNothing)
import GHC.Records (HasField)

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
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Bool
  , wbData :: BitVector XLen
  }
  deriving (Generic, NFDataX)

-- | 'ExMa' plus what the memory and CSR accesses produced.
data MaWb = MaWb
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
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Maybe Bool
  , wbData :: BitVector XLen
  , csrRdata :: Maybe (BitVector XLen)
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
  deriving (Generic, NFDataX)

-- | The @rs1@ and @rs2@ fields, shared by ID and the register file read.
srcRegs :: Inst -> (RegAddr, RegAddr)
srcRegs instBits = (slice d19 d15 instBits, slice d24 d20 instBits)

-- | The register this instruction writes
destReg ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
destReg stage = orNothing (isNothing stage.exception && stage.ctrl.rwbEn && stage.rdAddr /= 0) stage.rdAddr

forwardable ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  , HasField "wbData" stage t
  ) =>
  stage -> Maybe (RegAddr, t)
forwardable stage = (,stage.wbData) <$> destReg stage

unresolved ::
  ( HasField "exception" stage (Maybe (TrapCause, BitVector XLen))
  , HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
unresolved stage = orNothing (isLoad stage.ctrl || isCsrRead stage.ctrl) =<< destReg stage
