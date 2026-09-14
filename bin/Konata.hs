module Main (main) where

import Clash.Prelude
import Cuintet.Debug.Image (elfImage)
import Cuintet.Debug.Konata (konataLog)
import Cuintet.Debug.Sim (traceImage, upToEcall)
import System.Environment (getArgs)
import System.Exit (die)
import Prelude qualified as P

ramAddrWidth :: SNat 16
ramAddrWidth = SNat

budget :: Int
budget = 600000

main :: IO ()
main = do
  args <- getArgs
  case args of
    [elf] -> run elf
    _ -> die "usage: konata IMAGE.elf"

run :: FilePath -> IO ()
run elf = do
  img <- elfImage ramAddrWidth elf
  P.putStr $ P.unlines $ konataLog $ upToEcall $ traceImage budget img
