module Cuintet.Unit.BranchPredict (predict) where

import Clash.Prelude
import Cuintet.Eei (Addr, Inst)

predict :: Addr -> Inst -> Addr
predict addr _ = addr + 4
