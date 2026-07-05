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

-- | Outputs the memory request and the fetched instruction (if completed).
core ::
  (HiddenClockResetEnable dom) =>
  Signal dom (MemBusResp ILen) ->
  Signal dom (MemBusReq ILen XLen, Maybe (Addr, BitVector ILen))
core = mealy coreT initS
 where
  initS = CoreState{ifPc = 0, ifIsRequested = False, ifPcRequested = 0}
  coreT s@CoreState{..} MemBusResp{..} = (s', (req, fetched))
   where
    req = MemBusReq{valid, addr = ifPc, wen = False, wdata = errorX "wdata is invalid for instruction fetch."}
    fetched
      | ifIsRequested && rvalid = Just (ifPcRequested, rdata)
      | otherwise = Nothing
    valid = True
    busFree = not ifIsRequested || rvalid
    accepted = valid && ready && busFree
    s'
      | accepted = CoreState{ifPc = ifPc + 4, ifIsRequested = True, ifPcRequested = ifPc}
      | otherwise = s{ifIsRequested = ifIsRequested && not rvalid}
