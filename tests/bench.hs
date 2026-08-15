-- | Runs the benchmarks and prints what each cost, in cycles.
module Main (main) where

import Control.Monad (unless)
import Data.List (intercalate)
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

  printf "%-12s  %-16s  %10s  %s\n" "suite" "benchmark" "cycles" "result"
  passed <- mapM (uncurry report) [(s.name, o) | s <- selected, o <- s.outcomes]
  unless (and passed) exitFailure

report :: String -> (String, Outcome) -> IO Bool
report suite (name, outcome) = case outcome of
  Ok cycles -> row (show cycles) "ok" >> pure True
  Failed cycles err -> row (show cycles) ("FAIL: " <> err) >> pure False
  Hung err -> row "-" ("TIMEOUT: " <> err) >> pure False
  where
    row = printf "%-12s  %-16s  %10s  %s\n" suite name
