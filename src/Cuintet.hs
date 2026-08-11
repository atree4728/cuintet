{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet (vDom27, tangnano9k, system) where

import Clash.Prelude
import Cuintet.BusArbiter (BusArbiterReq (..), BusArbiterResp (..), busArbiter)
import Cuintet.Core (CoreIn (..), CoreOut (..), core)
import Cuintet.Eei (MemDataBytes)
import Cuintet.Ram (RamLane, initRamLanes, ram)

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

createDomain vSystem {vName = "Dom27", vPeriod = hzToPeriod 27e6, vResetPolarity = ActiveLow}

-- | top entity for Tang Nano 9k, see https://wiki.sipeed.com/hardware/en/tang/Tang-Nano-9K/Nano-9K.html
tangnano9k ::
  Clock Dom27 ->
  Reset Dom27 ->
  Signal Dom27 (BitVector 6)
tangnano9k clk rst =
  withClockResetEnable clk rst enableGen $
    complement . truncateB . (.led) <$> system (initRamLanes prog) -- BRAM size = 2^12 * 8 B = 32 KB
  where
    prog :: Vec 8 (BitVector 64)
    prog =
      0x24008093000f40b7
        :> 0x0011011300000113
        :> 0x800031f3fe209ee3
        :> 0x8001907300118193
        :> 0x0000006700000067
        :> 0
        :> 0
        :> 0
        :> Nil
{-# ANN
  tangnano9k
  ( Synthesize
      { t_name = "cuintet"
      , t_inputs = [PortName "clk", PortName "rst_n"]
      , t_output = PortName "led"
      }
  )
  #-}
{-# OPAQUE tangnano9k #-}
