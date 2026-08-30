module Main (main) where

import Clash.Prelude
import Cuintet.Debug.Sim (elfProgram, isEcall, retireImage)
import Cuintet.Debug.Spike (diverged, divergenceLines, withCommits)
import Cuintet.Pipeline (Retire (..))
import Data.Maybe (isNothing)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import Text.Printf (printf)
import Prelude qualified as P

ramAddrWidth :: SNat 14
ramAddrWidth = SNat

budget :: Int
budget = 20_000_000

-- | Retires to print either side of a mismatch.
context :: Int
context = 3

main :: IO ()
main = do
  args <- getArgs
  case args of
    [elf] -> run elf
    _ -> die "usage: tracediff IMAGE.elf"

run :: FilePath -> IO ()
run elf = do
  img <- elfProgram ramAddrWidth elf
  let ours = retireImage budget img
  case P.reverse ours of
    [] -> die (printf "%s: the core retired nothing" elf)
    l : _ | not (isEcall l) -> die (printf "%s: no ecall within %d cycles" elf budget)
    _ -> pure ()
  -- spike logs no line for a trapped instruction, the halting ecall included
  withCommits elf (report elf (P.filter (isNothing . (.trap)) ours))

report :: FilePath -> [Retire] -> [Retire] -> IO ()
report elf ours theirs = case diverged ours theirs of
  Nothing -> printf "%s: %d retires match spike\n" elf (P.length ours)
  Just d@(i, _, _) -> do
    printf "%s: mismatch at retire %d\n\n" elf i
    mapM_ putStrLn (divergenceLines context ours d)
    printf "\nRetire %d is the number the Konata log's R lines carry.\n" i
    exitFailure
