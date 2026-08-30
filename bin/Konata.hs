module Main (main) where

import Clash.Prelude
import Cuintet.Debug.Konata (konataLog)
import Cuintet.Debug.Image (hexImage)
import Cuintet.Debug.Sim (traceImage, upToEcall)
import System.Environment (getArgs)
import System.Exit (die)
import Prelude qualified as P

ramAddrWidth :: SNat 16
ramAddrWidth = SNat

budget :: Int
budget = 400000

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> run input
    _ -> die "usage: konata IMAGE.hex"

run :: FilePath -> IO ()
run input = do
  img <- hexImage ramAddrWidth <$> P.readFile input
  P.putStr $ P.unlines $ konataLog $ upToEcall $ traceImage budget img
