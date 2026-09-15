module Cuintet (system) where

import Clash.Prelude
import Cuintet.Core (CoreIn (..), CoreOut (..), core)
import Cuintet.Eei (MemDataBytes)
import Cuintet.Unit.Ram (RamLane, ram)

system ::
  ( HiddenClockResetEnable dom
  , KnownNat ramAddrWidth
  ) =>
  -- | RAM lane for each byte of the word
  Vec MemDataBytes (RamLane dom ramAddrWidth) ->
  Signal dom CoreOut
system lanes = coreOut
  where
    coreOut = core (CoreIn <$> iResp <*> dResp)
    (iResp, dResp) = ram lanes ((.iReq) <$> coreOut) ((.dReq) <$> coreOut)
