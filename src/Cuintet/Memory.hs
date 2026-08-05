module Cuintet.Memory (RamLane, memory, blockRamLanes, initRamLanes) where

import Clash.Prelude
import Cuintet.Eei
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

-- | A single byte lane of the memory.
newtype RamLane dom addrWidth
  = RamLane
      ( -- \| request address
        Signal dom (Unsigned addrWidth) ->
        -- \| written byte (Nothing when to load)
        Signal dom (Maybe (Unsigned addrWidth, BitVector 8)) ->
        -- \| single-cycle-delayed loaded byte
        Signal dom (BitVector 8)
      )

-- | One-cycle-delayed memory.
memory ::
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  ) =>
  -- | one-cycle-delayed BRAM component by @blockRam@ family, one per byte lane.
  Vec nBytes (RamLane dom addrWidth) ->
  -- | Memory read/write request.
  Signal dom (Maybe (MemBusReq nBytes addrWidth)) ->
  -- | Read value, requested at the previous clock.
  Signal dom (MemBusResp nBytes)
memory lanes req = MemBusResp True <$> rdata
  where
    laneWrite i mreq = do
      MemBusReq {addr, wdata} <- mreq
      StoreLanes bytes <- wdata
      (addr,) <$> bytes !! i

    runLane i (RamLane ram) = ram raddr (laneWrite i <$> req)

    raddr = maybe (errorX "memory: no request") (.addr) <$> req
    prevRdata = pack . reverse <$> bundle (imap runLane lanes)
    rready = delay False $ isJust <$> req
    rdata = orNothing <$> rready <*> prevRdata

-- | Uninitialized byte lanes, for synthesis.
blockRamLanes ::
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat addrWidth
  ) =>
  SNat depth ->
  Vec nBytes (RamLane dom addrWidth)
blockRamLanes depth = repeat (RamLane $ blockRamU NoClearOnReset depth)

-- | Byte lanes preloaded with a word-wise image, for simulation.
initRamLanes ::
  forall nBytes depth dom addrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat addrWidth
  ) =>
  Vec depth (BitVector (nBytes * 8)) ->
  Vec nBytes (RamLane dom addrWidth)
initRamLanes img = map (RamLane . blockRam . laneImage) indicesI
  where
    laneImage :: Index nBytes -> Vec depth (BitVector 8)
    laneImage i = map (\word -> reverse (bitCoerce word) !! i) img
