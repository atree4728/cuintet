-- | The 32 integer registers, as a RAM with two read ports and one write port.
module Cuintet.Unit.RegFile (RegReq (..), RegResp (..), regFile, mkRegReq) where

import Clash.Prelude
import Control.Arrow (first)
import Cuintet.Eei (RegAddr, XLen)
import Cuintet.Pipeline (IfId (..), srcRegs)

-- | The two registers to read this clock, and the write to apply at the end of it.
data RegReq = RegReq
  { rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , write :: Maybe (RegAddr, BitVector XLen)
  }
  deriving (Generic, NFDataX)

-- | What the two read ports hold this clock.
data RegResp = RegResp
  { rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

-- | One RAM per read port, both written with the same data.
regFile :: (HiddenClockResetEnable dom) => Signal dom RegReq -> Signal dom RegResp
regFile req = RegResp <$> port ((.rs1Addr) <$> req) <*> port ((.rs2Addr) <$> req)
  where
    zeroX0 0 _ = (0, 0)
    zeroX0 r d = (r, d)
    bypass write (rs, d)
      | Just (rd, wd) <- write, rs == rd = wd
      | otherwise = d
    wdata = (.write) <$> req
    port addr =
      bypass
        <$> wdata
        <*> ( zeroX0
                <$> addr
                <*> asyncRamPow2 (unpack <$> addr) (fmap (first unpack) <$> wdata)
            )

-- | The request for a given IF-ID FIFO head and WB write.
mkRegReq :: Maybe IfId -> Maybe (RegAddr, BitVector XLen) -> RegReq
mkRegReq entry write = RegReq {rs1Addr, rs2Addr, write}
  where
    (rs1Addr, rs2Addr) = maybe (0, 0) (srcRegs . (.instBits)) entry
