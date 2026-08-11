{- | The 32 integer registers, as a RAM with two read ports and one write port.

It sits beside 'Cuintet.Core.coreT' rather than inside it, so that it comes out
as a RAM instead of 32 registers' worth of flip-flops.

Reading is asynchronous, which is what lets ID have its operands on the clock it
decodes; writing takes effect at the next clock edge. Nothing ever writes @x0@,
since 'Cuintet.Pipeline.destReg' rejects it, but the RAM starts out undefined,
so reads of it are forced to zero.
-}
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
    zeroX0 0 _ = 0
    zeroX0 _ d = d
    wdata = fmap (first unpack) . (.write) <$> req
    port addr = zeroX0 <$> addr <*> asyncRamPow2 (unpack <$> addr) wdata
    _ = port $ (.rs1Addr) <$> req

{- | The request for a given IF-ID FIFO head and WB write.

The source addresses are taken from the raw instruction word, so the read does
not have to wait on decoding. An empty FIFO reads @x0@, which is harmless.
-}
mkRegReq :: Maybe IfId -> Maybe (RegAddr, BitVector XLen) -> RegReq
mkRegReq entry write = RegReq {rs1Addr, rs2Addr, write}
  where
    (rs1Addr, rs2Addr) = maybe (0, 0) (srcRegs . (.instBits)) entry
