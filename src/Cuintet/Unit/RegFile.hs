-- | The 32 integer registers, as one RAM bank per write port with a live value table naming the bank each read takes.
module Cuintet.Unit.RegFile (NRegs, ReadPorts, WritePorts, RegReq (..), RegResp (..), regFile) where

import Clash.Prelude
import Cuintet.Eei (RegAddr, XLen)

type NRegs = 32

type ReadPorts = 4

type WritePorts = 2

type Banked = Vec WritePorts (BitVector XLen)

type Lvt = Vec NRegs (Index WritePorts)

data RegReq = RegReq
  { rsAddrs :: Vec ReadPorts RegAddr
  , writes :: Vec WritePorts (Maybe (RegAddr, BitVector XLen))
  }
  deriving (Generic, NFDataX)

newtype RegResp = RegResp {rsData :: Vec ReadPorts (BitVector XLen)}
  deriving newtype (Generic, NFDataX)

regFile :: (HiddenClockResetEnable dom) => Signal dom RegReq -> Signal dom RegResp
regFile req = mealy regFileT (repeat 0) (bundle (req, banked))
  where
    writes = unbundle $ (.writes) <$> req
    rsAddrs = unbundle $ (.rsAddrs) <$> req
    readPort addr = bundle (asyncRamPow2 addr <$> writes)
    banked = bundle (readPort <$> rsAddrs)

regFileT :: Lvt -> (RegReq, Vec ReadPorts Banked) -> (Lvt, RegResp)
regFileT lvt (RegReq {..}, banked) =
  (lvt', RegResp (zipWith (readOut writes lvt) rsAddrs banked))
  where
    lvt' = ifoldl (\acc port write -> maybe acc (\(rd, _) -> replace rd port acc) write) lvt writes

readOut :: Vec WritePorts (Maybe (RegAddr, BitVector XLen)) -> Lvt -> RegAddr -> Banked -> BitVector XLen
readOut writes lvt rs banked = foldl bypass stored writes
  where
    stored = if rs == 0 then 0 else banked !! (lvt !! rs)
    bypass old = \case
      Just (rd, v) | rd == rs -> v
      _ -> old
