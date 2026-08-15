{-# OPTIONS_GHC -Wno-orphans #-}

{- | A top entity that exists only to be synthesised for timing.

It is not meant for any board: it is the core with a plain uninitialised memory,
so that neither a board's pin budget nor a preloaded image shapes the numbers.
'Cuintet.Top.TangNano9k.tangnano9k' carries an eight-word image, small enough for
synthesis to fold the memory away and report a critical path the core does not
really have.

The reported frequency is that of whatever device it is placed on, so read it as
a relative figure: what to watch is how it moves between commits.
-}
module Cuintet.Top.Timing (vDomTiming, timing) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..))
import Cuintet.Unit.Ram (blockRamLanes)

createDomain vSystem {vName = "DomTiming", vPeriod = hzToPeriod 100e6, vResetPolarity = ActiveLow}

-- | The core with 64 KiB of memory, reporting the LEDs so the pipeline is observed.
timing ::
  Clock DomTiming ->
  Reset DomTiming ->
  Signal DomTiming (BitVector 6)
timing clk rst =
  withClockResetEnable clk rst enableGen $
    truncateB . (.led) <$> system (blockRamLanes (SNat @13))
{-# ANN
  timing
  ( Synthesize
      { t_name = "cuintet_timing"
      , t_inputs = [PortName "clk", PortName "rst_n"]
      , t_output = PortName "led"
      }
  )
  #-}
{-# OPAQUE timing #-}
