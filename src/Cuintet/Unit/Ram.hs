module Cuintet.Unit.Ram (RamLane, ram, blockRamLanes, initRamLanes) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReadReq (..), BusReadResp (BusReadResp), BusWriteReq (..), BusWriteResp (BusWriteResp), StoreLanes (StoreLanes))
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

ram ::
  forall dom nBytes ramAddrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes -- to be a power of 2
  , KnownNat ramAddrWidth
  , 1 <= nBytes
  ) =>
  -- | one-cycle-delayed BRAM component by @blockRam@ family, one per byte lane.
  Vec nBytes (RamLane dom ramAddrWidth) ->
  -- | Instruction bus.
  Signal dom (Maybe BusReadReq) ->
  -- | Data read bus.
  Signal dom (Maybe BusReadReq) ->
  -- | Data write bus.
  Signal dom (Maybe (BusWriteReq nBytes)) ->
  -- | The word read on each read bus, requested at the previous clock; the write bus takes every write.
  (Signal dom (BusReadResp nBytes), Signal dom (BusReadResp nBytes), Signal dom BusWriteResp)
ram lanes iReq dReadReq dWriteReq = (copy iReq, copy dReadReq, pure $ BusWriteResp True)
  where
    toRamAddr :: Addr -> Unsigned ramAddrWidth
    toRamAddr a = resize (a `shiftR` natToNum @(CLog 2 nBytes))

    laneWrite laneIndex mreq = do
      BusWriteReq {addr, wdata = StoreLanes bytes} <- mreq
      (toRamAddr addr,) <$> bytes !! laneIndex

    copy req = BusReadResp True <$> rdata
      where
        runLane laneIndex (RamLane lane) = lane raddr (laneWrite laneIndex <$> dWriteReq)
        raddr = maybe (errorX "memory: no request") (toRamAddr . (.addr)) <$> req
        prevRdata = pack . reverse <$> bundle (imap runLane lanes)
        rready = delay False (isJust <$> req)
        rdata = orNothing <$> rready <*> prevRdata

-- | Uninitialized byte lanes of a given size.
blockRamLanes ::
  forall dom nBytes ramAddrWidth.
  ( HiddenClockResetEnable dom
  , KnownNat nBytes
  , KnownNat ramAddrWidth
  ) =>
  SNat ramAddrWidth ->
  Vec nBytes (RamLane dom ramAddrWidth)
blockRamLanes ramAddrWidth = repeat (RamLane $ blockRamU NoClearOnReset (pow2SNat ramAddrWidth))

-- | Byte lanes preloaded with a word-wise image, sized after it.
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
