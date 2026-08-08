{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet (vDom50, topEntity, system) where

import Clash.Prelude
import Cuintet.Core (CoreIn (..), CoreOut (..), core)
import Cuintet.Eei (MemDataBytes)
import Cuintet.MemArbiter (MemArbiterReq (..), MemArbiterResp (..), memArbiter)
import Cuintet.Memory (RamLane, blockRamLanes, memory)
import Cuintet.Pipeline (MaWb)

createDomain vSystem {vName = "Dom50", vPeriod = hzToPeriod 50e6}

system ::
  ( HiddenClockResetEnable dom
  , KnownNat ramAddrWidth
  ) =>
  -- | RAM lane for each byte of the word
  Vec MemDataBytes (RamLane dom ramAddrWidth) ->
  Signal dom (Maybe MaWb)
system lanes = (.instLog) <$> coreOut
  where
    coreOut = core coreIn
    coreIn = mkCoreIn <$> arbResp
    mkCoreIn MemArbiterResp {iResp, dResp} = CoreIn {iResp, dResp}

    arbReq = mkArbReq <$> coreOut <*> memResp
    mkArbReq CoreOut {iReq, dReq} mr = MemArbiterReq {iReq, dReq, memResp = mr}
    arbResp = memArbiter arbReq

    memReq = (.memReq) <$> arbResp
    memResp = memory lanes memReq

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Maybe MaWb)
topEntity = exposeClockResetEnable $ system (blockRamLanes d19) -- BRAM size = 2^19 * 8 bytes = 4 MiB
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "cuintet"
      , t_inputs =
          [ PortName "CLK"
          , PortName "RST"
          , PortName "EN"
          ]
      , t_output = PortName "DOUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}
