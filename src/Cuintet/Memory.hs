module Cuintet.Memory where

import Clash.Prelude
import Cuintet.Eei
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

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
  Signal dom (Maybe (MemBusReq dataWidth addrWidth)) ->
  -- | Read value, requested at the previous clock.
  Signal dom (MemBusResp dataWidth)
memory ram req = MemBusResp True <$> rdata
 where
  write mreq = do
    MemBusReq{addr, wdata} <- mreq
    (addr,) <$> wdata

  raddr = maybe (errorX "memory: no request") (.addr) <$> req
  prevRdata = ram raddr (write <$> req)
  rready = delay False $ isJust <$> req
  rdata = orNothing <$> rready <*> prevRdata
