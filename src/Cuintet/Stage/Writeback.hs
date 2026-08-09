module Cuintet.Stage.Writeback (WritebackIn (..), WritebackOut (..), writeback, initRegFile) where

import Clash.Prelude
import Cuintet.Eei (RegFile)
import Cuintet.Pipeline (MaWb (..), pendingRd)

newtype WritebackIn = WriteBackIn {entry :: Maybe MaWb}

newtype WritebackOut = WriteBackOut {retired :: Maybe MaWb}

initRegFile :: RegFile
initRegFile = zeroBits :> replicate d31 (deepErrorX "register uninitialized")

writeback :: RegFile -> WritebackIn -> (RegFile, WritebackOut)
writeback regFile WriteBackIn {entry} = (regFile', WriteBackOut {retired = entry})
  where
    regFile' = case entry of
      Just maWb | Just rdAddr <- pendingRd maWb -> replace rdAddr maWb.wbData regFile
      _ -> regFile
