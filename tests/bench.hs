-- | Runs the benchmarks and prints what each cost, in cycles and IPC.
module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
import Programs (Outcome (..), Stats (..), Suite (..), benchmarks, ipc)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import Text.Printf (printf)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  selected <- case filter ((`elem` args) . (.name)) benchmarks of
    _ | null args -> pure benchmarks
    [] -> die ("no such benchmark suite; have " <> intercalate ", " (map (.name) benchmarks))
    picked -> pure picked

  printf "%-12s  %-16s  %10s  %5s  %s\n" "suite" "benchmark" "cycles" "ipc" "result"
  passed <- mapM (uncurry report) [(s.name, o) | s <- selected, o <- s.outcomes]
  unless (and passed) exitFailure

report :: String -> (String, Outcome) -> IO Bool
report suite (name, outcome) = case outcome of
  Ok stats -> row (cost stats) "ok" >> pure True
  Failed stats err -> row (cost stats) ("FAIL: " <> err) >> pure False
  Hung err -> row ("-", "-") ("TIMEOUT: " <> err) >> pure False
  where
    cost stats = (show stats.cycles, printf "%.3f" (ipc stats))
    row (cycles, rate) = printf "%-12s  %-16s  %10s  %5s  %s\n" suite name cycles rate
