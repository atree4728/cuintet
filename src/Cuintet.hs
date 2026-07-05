{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet where

import Clash.Prelude
import Cuintet.Core (core)
import Cuintet.Eei
import Cuintet.Memory (memory)

createDomain vSystem{vName = "Dom50", vPeriod = hzToPeriod 50e6}

system ::
  (HiddenClockResetEnable dom) =>
  ( Signal dom (Unsigned MemAddrWidth) ->
    Signal dom (Maybe (Unsigned MemAddrWidth, BitVector ILen)) ->
    Signal dom (BitVector ILen)
  ) ->
  Signal dom (Maybe (Addr, BitVector ILen))
system ram = fetched
 where
  (coreReq, fetched) = unbundle $ core memResp
  memResp = memory ram $ toMemReq <$> coreReq
  toMemReq MemBusReq{..} =
    MemBusReq
      { valid
      , addr = addrToMemAddr addr
      , wen = False
      , wdata = errorX "wdata is invalid for instruction fetch."
      }

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Maybe (Addr, BitVector ILen))
topEntity = exposeClockResetEnable $ system $ blockRam1 NoClearOnReset (SNat @(2 ^ MemAddrWidth)) 0
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
