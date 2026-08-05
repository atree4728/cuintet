{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet where

import Clash.Prelude
import Cuintet.Core (CoreIn (..), CoreOut (..), InstLog, core)
import Cuintet.Eei
import Cuintet.MemArbiter (MemArbiterReq (..), MemArbiterResp (..), memArbiter)
import Cuintet.Memory (RamUnit, blockRamLanes, memory)

createDomain vSystem{vName = "Dom50", vPeriod = hzToPeriod 50e6}

system ::
  (HiddenClockResetEnable dom) =>
  -- | ram unit for each byte lane
  Vec (Div MemDataWidth 8) (RamUnit dom MemAddrWidth) ->
  Signal dom (Maybe InstLog)
system rams = (.instLog) <$> coreOut
 where
  coreOut = core coreIn
  coreIn = mkCoreIn <$> arbResp
  mkCoreIn MemArbiterResp{iResp, dResp} = CoreIn{iResp, dResp}

  arbReq = mkArbReq <$> coreOut <*> memResp
  mkArbReq CoreOut{iReq, dReq} mr = MemArbiterReq{iReq, dReq, memResp = mr}
  arbResp = memArbiter arbReq

  memReq = (.memReq) <$> arbResp
  memResp = memory rams memReq

topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Maybe InstLog)
topEntity = exposeClockResetEnable $ system (blockRamLanes (SNat @(2 ^ MemAddrWidth)))
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
