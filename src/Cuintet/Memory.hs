module Cuintet.Memory where

import Clash.Prelude
import Cuintet.Eei
import Data.Function (applyWhen)

memory ::
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  ) =>
  Vec (2 ^ addrWidth) (BitVector dataWidth) ->
  Signal dom (MemBusReq dataWidth addrWidth) ->
  Signal dom (MemBusResp dataWidth)
memory = mealy memoryT
 where
  memoryT mem MemoryReq{..} = (mem', MemoryResp{ready = True, rvalid = valid, rdata})
   where
    rdata = mem !! addr
    mem' = applyWhen (valid && wen) (replace addr wdata) mem
