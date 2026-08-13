module Cuintet.Unit.BranchPredict (predict) where

import Clash.Prelude
import Cuintet.Eei (Addr, Inst, Opcode (..))
import Cuintet.Stage.Decode (immB, immJ)

predict :: Addr -> Inst -> Addr
predict pc instBits
  | JAL <- opcode = pc + numConvert (immJ instBits)
  | BRANCH <- opcode, backward = pc + numConvert (immB instBits)
  | otherwise = pc + 4
  where
    opcode = unpack (slice d6 d0 instBits)
    backward = msb instBits == high
