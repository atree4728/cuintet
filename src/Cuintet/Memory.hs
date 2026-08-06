module Cuintet.Memory (RamLane, memory, blockRamLanes, initRamLanes) where

import Clash.Prelude
import Cuintet.Eei
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

-- | A single byte lane of the memory.
newtype RamLane dom ramAddrWidth
  = RamLane
      ( -- \| request address
        Signal dom (Unsigned ramAddrWidth) ->
        -- \| written byte (Nothing when to load)
        Signal dom (Maybe (Unsigned ramAddrWidth, BitVector 8)) ->
        -- \| single-cycle-delayed loaded byte
        Signal dom (BitVector 8)
      )

-- | One-cycle-delayed memory.
memory ::
  forall dom nBytes ramAddrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes -- to be a power of 2
  , KnownNat ramAddrWidth
  , 1 <= nBytes
  ) =>
  -- | one-cycle-delayed BRAM component by @blockRam@ family, one per byte lane.
  Vec nBytes (RamLane dom ramAddrWidth) ->
  -- | Memory read/write request.
  Signal dom (Maybe (MemBusReq nBytes)) ->
  -- | Read value, requested at the previous clock.
  Signal dom (MemBusResp nBytes)
memory lanes req = MemBusResp True <$> rdata
  where
    toRamAddr :: Addr -> Unsigned ramAddrWidth
    toRamAddr a = resize (a `shiftR` natToNum @(CLog 2 nBytes))

    laneWrite laneIndex mreq = do
      MemBusReq {addr, wdata} <- mreq
      StoreLanes bytes <- wdata
      (toRamAddr addr,) <$> bytes !! laneIndex

    runLane laneIndex (RamLane ram) = ram raddr (laneWrite laneIndex <$> req)
      where
        raddr = maybe (errorX "memory: no request") (toRamAddr . (.addr)) <$> req

    prevRdata = pack . reverse <$> bundle (imap runLane lanes)
    rready = delay False $ isJust <$> req
    rdata = orNothing <$> rready <*> prevRdata

-- | Uninitialized byte lanes, for synthesis.
blockRamLanes ::
  forall dom nBytes ramAddrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat ramAddrWidth
  ) =>
  SNat ramAddrWidth ->
  Vec nBytes (RamLane dom ramAddrWidth)
blockRamLanes ramAddrWidth = repeat (RamLane $ blockRamU NoClearOnReset (pow2SNat ramAddrWidth))

-- | Byte lanes preloaded with a word-wise image, for simulation.
initRamLanes ::
  forall nBytes dom ramAddrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat ramAddrWidth
  ) =>
  Vec (2 ^ ramAddrWidth) (BitVector (nBytes * 8)) ->
  Vec nBytes (RamLane dom ramAddrWidth)
initRamLanes img = map (RamLane . blockRam . laneImage) indicesI
  where
    laneImage i = map (\word -> reverse (bitCoerce word) !! i) img
