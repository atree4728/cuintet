module Cuintet.Stage.Writeback (writeback) where

import Clash.Prelude
import Cuintet.Eei (RegFile, XLen)
import Cuintet.Pipeline (MaWb (..))

writeback :: RegFile -> Maybe MaWb -> Vec 32 (BitVector XLen)
writeback regFile instLogM = maybe regFile (\(a, d) -> replace a d regFile) (instLogM >>= (.wbReq))
