module Cuintet.Unit.RegFile (ReadPorts, RegReq (..), RegResp (..), regFile) where

import Clash.Prelude
import Cuintet.Eei (DispatchWidth, PRegAddr, WriteBackWidth, XLen)
import Cuintet.Unit.MultiRam (multiRam)

type ReadPorts = 2 * DispatchWidth

data RegReq = RegReq
  { rsAddrs :: Vec ReadPorts PRegAddr
  , writes :: Vec WriteBackWidth (Maybe (PRegAddr, BitVector XLen))
  }
  deriving (Generic, NFDataX)

newtype RegResp = RegResp {rsData :: Vec ReadPorts (BitVector XLen)}
  deriving newtype (Generic, NFDataX)

regFile :: (HiddenClockResetEnable dom) => Signal dom RegReq -> Signal dom RegResp
regFile req = regOutput <$> req <*> multiRam ((.rsAddrs) <$> req) ((.writes) <$> req)

regOutput :: RegReq -> Vec ReadPorts (BitVector XLen) -> RegResp
regOutput RegReq {..} stored = RegResp $ zipWith readOut rsAddrs stored
  where
    readOut rs raw = foldl (bypass rs) (if rs == 0 then 0 else raw) writes
    bypass rs old = \case
      Just (rd, v) | rd == rs -> v
      _ -> old
