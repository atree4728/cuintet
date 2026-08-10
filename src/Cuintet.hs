{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet (vDom50, topEntity, system) where

import Clash.Prelude
import Cuintet.BusArbiter (BusArbiterReq (..), BusArbiterResp (..), busArbiter)
import Cuintet.Core (CoreIn (..), CoreOut (..), core)
import Cuintet.Eei (MemDataBytes)
import Cuintet.Ram (RamLane, blockRamLanes, ram)

system ::
  ( HiddenClockResetEnable dom
  , KnownNat ramAddrWidth
  ) =>
  -- | RAM lane for each byte of the word
  Vec MemDataBytes (RamLane dom ramAddrWidth) ->
  Signal dom CoreOut
system lanes = coreOut
  where
    coreOut = core coreIn
    coreIn = mkCoreIn <$> arbResp
    mkCoreIn BusArbiterResp {iResp, dResp} = CoreIn {iResp, dResp}

    arbReq = mkArbReq <$> coreOut <*> memResp
    mkArbReq CoreOut {iReq, dReq} mr = BusArbiterReq {iReq, dReq, memResp = mr}
    arbResp = busArbiter arbReq

    memReq = (.memReq) <$> arbResp
    memResp = ram lanes memReq

createDomain vSystem {vName = "Dom50", vPeriod = hzToPeriod 27e6}

-- | top entity for Tang Nano 9k, see https://wiki.sipeed.com/hardware/en/tang/Tang-Nano-9K/Nano-9K.html
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (BitVector 6)
topEntity = exposeClockResetEnable $ truncateB . (.led) <$> system (blockRamLanes d12) -- BRAM size = 2^12 * 8 B = 32 KB
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
