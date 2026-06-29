module Cuintet.Core where

import Clash.Prelude
import Cuintet.Eei

-- | "if" is a shorthand of instruction fetch.
data CoreState
  = CoreState
  { ifPc :: Addr
  -- ^ Program counter.
  , ifIsRequested :: Bool
  -- ^ Whether to be fetching.
  , ifPcRequested :: Addr
  -- ^ Address being fetched.
  }
  deriving (Generic, NFDataX)

core ::
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  ) =>
  Signal dom (MemBusResp dataWidth) ->
  Signal dom (MemBusReq dataWidth addrWidth)
core = mealy coreT CoreState{ifPc = 0, ifIsRequested = False, ifPcRequested = 0}
 where
  coreT CoreState{..} MemoryResp{..} =
    (next, MemoryReq{valid = True, addr = undefined, wen = False, wdata = 0})
   where
    ifPcNext = ifPc + 4
    next
      | ifIsRequested && rvalid = CoreState{ifPc = ifPcNext, ifIsRequested, ifPcRequested = ifPc}
      | ifIsRequested = CoreState{ifIsRequested = ready, ..}
      | ready = CoreState{ifPc = ifPcNext, ifIsRequested = True, ifPcRequested = ifPc}
      | otherwise = CoreState{..}
