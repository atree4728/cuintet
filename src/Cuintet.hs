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

{- | top entity for Tang Nano 9k, see https://wiki.sipeed.com/hardware/en/tang/Tang-Nano-9K/Nano-9K.html

Both the reset and the LEDs are active low on the board, hence 'ActiveLow' on
'Dom27' and the 'complement' here.
-}
tangnano9k ::
  Clock Dom27 ->
  Reset Dom27 ->
  Signal Dom27 (BitVector 6)
tangnano9k clk rst =
  withClockResetEnable clk rst enableGen $
    complement . truncateB . (.led) <$> system (initRamLanes prog)
  where
    -- The whole memory: two instructions per word, the earlier one in the low half.
    -- It counts up on the LEDs, one step per pass of a 10^6-iteration delay loop.
    --
    --       lui   x1, 0xf4        -- x1 = 1000000
    --       addi  x1, x1, 0x240
    --       addi  x2, x0, 0
    -- loop: addi  x2, x2, 1
    --       bne   x1, x2, loop
    --       csrrc x3, 0x800, x0   -- x3 = led
    --       addi  x3, x3, 1
    --       csrrw x0, 0x800, x3   -- led = x3
    --       jalr  x0, 0(x0)       -- start over
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
