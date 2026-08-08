module Cuintet.Pipeline (IfId (..), IdEx (..), InstLog (..), rdOf, idExRd) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, Inst, XLen)
import Cuintet.Util (orNothing)

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
  , rs1Addr :: BitVector 5
  , rs2Addr :: BitVector 5
  , rdAddr :: BitVector 5
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

data InstLog = InstLog
  { pc :: Addr
  , inst :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: BitVector 5
  , rs2Addr :: BitVector 5
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Maybe Bool
  , wbReq :: Maybe (BitVector 5, BitVector XLen)
  , csrRdata :: Maybe (BitVector XLen)
  }
  deriving (Generic, NFDataX)

rdOf :: InstCtrl -> BitVector 5 -> Maybe (BitVector 5)
rdOf InstCtrl {rwbEn} rdAddr = orNothing (rwbEn && rdAddr /= 0) rdAddr

idExRd :: Maybe IdEx -> Maybe (BitVector 5)
idExRd idExM = idExM >>= \idEx -> rdOf idEx.ctrl idEx.rdAddr
