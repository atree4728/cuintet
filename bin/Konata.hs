module Main (main) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..), CoreTrace (..))
import Cuintet.CoreCtrl (InstCtrl (systemOp))
import Cuintet.Debug.Konata (konataLog)
import Cuintet.Eei (MemDataBytes, SystemOp (..))
import Cuintet.Image (hexImage)
import Cuintet.Pipeline (MaWb (ctrl))
import Cuintet.Unit.Ram (initRamLanes)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (replaceExtension, takeFileName)
import System.IO (hPutStrLn, stderr)
import Prelude qualified as P

type RamAddrWidth = 16

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> run input (replaceExtension (takeFileName input) ".kanata.log")
    [input, output] -> run input output
    _ -> die "usage: konata IMAGE.hex [OUT]"

run :: FilePath -> FilePath -> IO ()
run input output = do
  img <- hexImage input <$> P.readFile input
  let traces = sampleWithResetN @System d1 200000 $ (.trace) <$> system (initRamLanes @MemDataBytes @_ @RamAddrWidth img)
  P.writeFile output $ P.unlines $ konataLog $ upToEcall traces
  hPutStrLn stderr (output <> ": written")

upToEcall :: [CoreTrace] -> [CoreTrace]
upToEcall = P.foldr (\t rest -> t : if isEcall t then [] else rest) []
  where
    isEcall t
      | Just l <- t.instLog, Just SysEcall <- l.ctrl.systemOp = True
      | otherwise = False
