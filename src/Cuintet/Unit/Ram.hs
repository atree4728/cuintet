module Cuintet.Unit.Ram (RamLane, ram, blockRamLanes, initRamLanes) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (BusResp), StoreLanes (StoreLanes))
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
  Signal dom (Maybe (BusReq nBytes)) ->
  -- | Data read bus.
  Signal dom (Maybe (BusReq nBytes)) ->
  -- | Data write bus.
  Signal dom (Maybe (BusReq nBytes)) ->
  -- | The word read on each read bus, requested at the previous clock; the write bus takes every write.
  (Signal dom (BusResp nBytes), Signal dom (BusResp nBytes), Signal dom (BusResp nBytes))
ram lanes iReq dReadReq dWriteReq = (copy iReq, copy dReadReq, pure $ BusResp True Nothing)
  where
    toRamAddr :: Addr -> Unsigned ramAddrWidth
    toRamAddr a = resize (a `shiftR` natToNum @(CLog 2 nBytes))

    laneWrite laneIndex mreq = do
      BusReq {addr, wdata} <- mreq
      StoreLanes bytes <- wdata
      (toRamAddr addr,) <$> bytes !! laneIndex

    copy req = BusResp True <$> rdata
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
