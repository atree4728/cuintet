module Main (main) where

import Clash.Prelude
import Cuintet.Debug.Konata (konataLog)
import Cuintet.Debug.Sim (hexProgram, traceImage)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (replaceExtension, takeFileName)
import System.IO (hPutStrLn, stderr)
import Prelude qualified as P

ramAddrWidth :: SNat 16
ramAddrWidth = SNat

budget :: Int
budget = 400000

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> run input (replaceExtension (takeFileName input) ".kanata.log")
    [input, output] -> run input output
    _ -> die "usage: konata IMAGE.hex [OUT]"

run :: FilePath -> FilePath -> IO ()
run input output = do
  img <- hexProgram ramAddrWidth input <$> P.readFile input
  P.writeFile output $ P.unlines $ konataLog $ traceImage budget img
  hPutStrLn stderr (output <> ": written")
