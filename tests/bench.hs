-- | Runs the benchmarks and prints what each cost, in cycles and IPC.
module Main (main) where

import Control.Monad (unless)
import Cuintet.Debug.Sim (Run (..), ipc)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Programs (Outcome (..), Suite (..), benchmarks)
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

report :: String -> Outcome -> IO Bool
report suite o = do
  printf "%-12s  %-16s  %10d  %5.3f  %s\n" suite o.name o.run.cycles (ipc o.run) result
  pure (isNothing o.failure)
  where
    result = maybe "ok" ("FAIL: " <>) o.failure
