{-# OPTIONS_GHC -Wno-orphans #-}

module Cuintet where

import Clash.Prelude
import Cuintet.Core (core)
import Cuintet.Corectrl (InstCtrl)
import Cuintet.Eei
import Cuintet.Memory (memory)

createDomain vSystem{vName = "Dom50", vPeriod = hzToPeriod 50e6}

system ::
  (HiddenClockResetEnable dom) =>
  ( Signal dom MemAddr ->
    Signal dom (Maybe (MemAddr, Inst)) ->
    Signal dom Inst
  ) ->
  Signal dom (Maybe (Addr, InstCtrl, BitVector XLen))
system ram = instInfo
 where
  (coreReq, instInfo) = unbundle $ core memResp
  memResp = memory ram $ toMemReq <$> coreReq
  toMemReq MemBusReq{..} =
    MemBusReq
      { valid
      , addr = addrToMemAddr addr
      , wdata
      }

{- |
>>> sampleHex = 0x01234567 :> 0x89abcdef :> 0xdeadbeef :> 0xcafebebe :> Nil
>>> sampleN @System 7 $ system (blockRam sampleHex)
[Nothing,Nothing,Nothing,Nothing,Just (0,InstCtrl {itype = IType, rwbEn = True, isLui = False, isAluOp = False, isJump = True, isLoad = False, funct3 = 0b100, funct7 = 0b000_0000},0b0000_0000_0000_0000_0000_0000_0001_0010),Just (4,InstCtrl {itype = JType, rwbEn = True, isLui = False, isAluOp = False, isJump = True, isLoad = False, funct3 = 0b100, funct7 = 0b100_0100},0b1111_1111_1111_1101_1110_0000_0100_1101),Just (8,InstCtrl {itype = JType, rwbEn = True, isLui = False, isAluOp = False, isJump = True, isLoad = False, funct3 = 0b011, funct7 = 0b110_1111},0b1111_1111_1111_1110_1101_1010_1111_0101)]
-}
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 (Maybe (Addr, InstCtrl, BitVector XLen))
topEntity = exposeClockResetEnable $ system $ blockRamU NoClearOnReset (SNat @(2 ^ MemAddrWidth))
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
