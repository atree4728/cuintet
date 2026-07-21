{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet where

import Clash.Prelude
import Cuintet.Core (InstLog, core)
import Cuintet.Eei
import Cuintet.Memory (memory)

createDomain vSystem{vName = "Dom50", vPeriod = hzToPeriod 50e6}

system ::
  (HiddenClockResetEnable dom) =>
  ( Signal dom MemAddr ->
    Signal dom (Maybe (MemAddr, Inst)) ->
    Signal dom Inst
  ) ->
  Signal dom (Maybe InstLog)
system ram = instInfo
 where
  (coreReq, instInfo) = unbundle $ core memResp
  memResp = memory ram $ fmap toMemReq <$> coreReq
  toMemReq MemBusReq{..} = MemBusReq{addr = addrToMemAddr addr, wdata}

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Maybe InstLog)
topEntity = exposeClockResetEnable $ system (blockRamU NoClearOnReset (SNat @(2 ^ MemAddrWidth)))
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
