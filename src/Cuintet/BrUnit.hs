module Cuintet.BrUnit (brUnit) where

import Clash.Prelude
import Cuintet.Eei (XLen)

brUnit :: BitVector 3 -> BitVector XLen -> BitVector XLen -> Bool
brUnit funct3 op1 op2 = case funct3 of
  0b000 -> beq
  0b001 -> not beq
  0b100 -> blt
  0b101 -> not blt
  0b110 -> bltu
  0b111 -> not bltu
  _ -> False
  where
    beq = op1 == op2
    blt = (bitCoerce op1 :: Signed XLen) < (bitCoerce op2 :: Signed XLen)
    bltu = (bitCoerce op1 :: Unsigned XLen) < (bitCoerce op2 :: Unsigned XLen)
