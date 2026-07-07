-- | BRAM for instruction fetch.
module Cuintet.Memory where

import Clash.Prelude
import Cuintet.Eei
import Cuintet.Util (orNothing)

-- | One-cycle-delayed memory.
memory ::
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  ) =>
  -- | one-cycle-delayed BRAM component by @blockRam@ family.
  ( Signal dom (Unsigned addrWidth) ->
    Signal dom (Maybe (Unsigned addrWidth, BitVector dataWidth)) ->
    Signal dom (BitVector dataWidth)
  ) ->
  -- | Memory read/write request.
  Signal dom (MemBusReq dataWidth addrWidth) ->
  -- | Read value, requested at the previous clock.
  Signal dom (MemBusResp dataWidth)
memory ram req = MemBusResp True <$> rdata
 where
  write MemBusReq{..}
    | valid, Just wd <- wdata = Just (addr, wd)
    | otherwise = Nothing

  prevRdata = ram ((.addr) <$> req) (write <$> req)
  rready = delay False $ (.valid) <$> req
  rdata = orNothing <$> rready <*> prevRdata
