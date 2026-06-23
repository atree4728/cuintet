{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet.Cpu where

import Clash.Prelude

createDomain vSystem{vName = "Dom50", vPeriod = hzToPeriod 50e6}

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Unsigned 8) ->
  Signal Dom50 (Unsigned 8)
topEntity = exposeClockResetEnable accum
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "cuintet"
      , t_inputs =
          [ PortName "CLK"
          , PortName "RST"
          , PortName "EN"
          , PortName "DIN"
          ]
      , t_output = PortName "DOUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}

accum ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned n)
accum = mealy accumT 0
 where
  accumT s i = (s + i, s)
