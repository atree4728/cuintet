module Cuintet.Pipeline (IfId (..), IdEx (..), ExMa (..), MaWb (..), pendingRd) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, Inst, RegAddr, XLen)
import Cuintet.Util (orNothing)
import GHC.Records (HasField)

data IfId = IfId
  { pc :: Addr
  , instBits :: Inst
  }
  deriving (Generic, NFDataX)

data IdEx = IdEx
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

data ExMa = ExMa
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Bool
  , wbData :: BitVector XLen
  }
  deriving (Generic, NFDataX)

data MaWb = MaWb
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Maybe Bool
  , wbReq :: Maybe (RegAddr, BitVector XLen)
  , csrRdata :: Maybe (BitVector XLen)
  }
  deriving (Generic, NFDataX)

pendingRd ::
  ( HasField "ctrl" stage InstCtrl
  , HasField "rdAddr" stage RegAddr
  ) =>
  stage -> Maybe RegAddr
pendingRd stage = orNothing (stage.ctrl.rwbEn && stage.rdAddr /= 0) stage.rdAddr
