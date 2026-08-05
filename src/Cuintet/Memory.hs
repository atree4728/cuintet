module Cuintet.Memory where

import Clash.Prelude
import Cuintet.Eei
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

-- | A single byte lane of the memory.
newtype RamUnit dom addrWidth
  = RamUnit
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
  , KnownNat dataBytes
  , KnownNat addrWidth
  ) =>
  -- | one-cycle-delayed BRAM component by @blockRam@ family, one per byte lane.
  Vec dataBytes (RamUnit dom addrWidth) ->
  -- | Memory read/write request.
  Signal dom (Maybe (MemBusReq (dataBytes * 8) addrWidth)) ->
  -- | Read value, requested at the previous clock.
  Signal dom (MemBusResp (dataBytes * 8))
memory rams req = MemBusResp True <$> rdata
 where
  laneWrite i mreq = do
    MemBusReq{addr, wdata} <- mreq
    StoreFmt lanes <- wdata
    (addr,) <$> lanes !! i

  runLane i (RamUnit ram) = ram raddr (laneWrite i <$> req)

  raddr = maybe (errorX "memory: no request") (.addr) <$> req
  prevRdata = pack . reverse <$> bundle (imap runLane rams)
  rready = delay False $ isJust <$> req
  rdata = orNothing <$> rready <*> prevRdata

-- | Uninitialized byte lanes, for synthesis.
blockRamLanes ::
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat addrWidth
  , 1 <= depth
  ) =>
  SNat depth ->
  Vec nBytes (RamUnit dom addrWidth)
blockRamLanes depth = repeat (RamUnit $ blockRamU NoClearOnReset depth)

-- | Byte lanes preloaded with a word-wise image, for simulation.
initRamLanes ::
  forall nBytes depth dom addrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat addrWidth
  , KnownNat depth
  , 1 <= depth
  ) =>
  Vec depth (BitVector (nBytes * 8)) ->
  Vec nBytes (RamUnit dom addrWidth)
initRamLanes img = map (RamUnit . blockRam . laneImage) indicesI
 where
  laneImage :: Index nBytes -> Vec depth (BitVector 8)
  laneImage i = map (\word -> reverse (bitCoerce word) !! i) img
