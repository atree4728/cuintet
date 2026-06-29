{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet where

import Clash.Prelude

createDomain vSystem{vName = "Dom50", vPeriod = hzToPeriod 50e6}

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  ()
topEntity = exposeClockResetEnable ()
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
