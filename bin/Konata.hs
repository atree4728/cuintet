module Main (main) where

import Clash.Prelude
import Cuintet.Debug.Konata (konataLog)
import Cuintet.Debug.Sim (hexProgram, traceImage)
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
  img <- hexProgram ramAddrWidth input <$> P.readFile input
  P.putStr $ P.unlines $ konataLog $ traceImage budget img
