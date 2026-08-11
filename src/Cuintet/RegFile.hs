module Cuintet.RegFile (RegReq (..), RegResp (..), regFile, mkRegReq) where

import Clash.Prelude
import Control.Arrow (first)
import Cuintet.Eei (RegAddr, XLen)
import Cuintet.Pipeline (IfId (..), srcRegs)

data RegReq = RegReq
  { rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , write :: Maybe (RegAddr, BitVector XLen)
  }
  deriving (Generic, NFDataX)

data RegResp = RegResp
  { rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

regFile :: (HiddenClockResetEnable dom) => Signal dom RegReq -> Signal dom RegResp
regFile req = RegResp <$> port ((.rs1Addr) <$> req) <*> port ((.rs2Addr) <$> req)
  where
    zeroX0 0 _ = 0
    zeroX0 _ d = d
    wdata = fmap (first unpack) . (.write) <$> req
    port addr = zeroX0 <$> addr <*> asyncRamPow2 (unpack <$> addr) wdata
    _ = port $ (.rs1Addr) <$> req

mkRegReq :: Maybe IfId -> Maybe (RegAddr, BitVector XLen) -> RegReq
mkRegReq entry write = RegReq {rs1Addr, rs2Addr, write}
  where
    (rs1Addr, rs2Addr) = maybe (0, 0) (srcRegs . (.instBits)) entry
