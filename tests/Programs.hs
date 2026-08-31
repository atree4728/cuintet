-- | The programs under @programs\/@, and what running each one came to.
module Programs (Suite (..), Outcome (..), riscvTests, benchmarks) where

import Clash.Prelude
import Cuintet.Debug.Image (binImage)
import Cuintet.Debug.Sim (Run (..), runImage)
import Cuintet.Eei (RegFile)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (sortOn)
import System.FilePath (takeBaseName)
import Text.Printf (printf)

data Suite = Suite
  { name :: String
  , outcomes :: [Outcome]
  }

-- | What running one image came to.
data Outcome = Outcome
  { name :: String
  , run :: Run
  , failure :: Maybe String
  }

-- | Runs every image in a directory.
suite ::
  (KnownNat ramAddrWidth) =>
  String -> SNat ramAddrWidth -> Int -> (RegFile -> Maybe String) -> [(FilePath, BS.ByteString)] -> Suite
suite name ramAddrWidth budget verdict images =
  Suite {name, outcomes = [outcome path bs | (path, bs) <- sortOn fst images]}
  where
    outcome path bs = Outcome {name = takeBaseName path, run, failure}
      where
        run = runImage budget (binImage ramAddrWidth bs)
        failure
          | run.halted = verdict run.regs
          | otherwise = Just (printf "no ecall within %d cycles" budget)

riscvTests :: Suite
riscvTests =
  suite "riscv-tests" (SNat @11) 200_000 fromTestnum $(makeRelativeToProject "programs/riscv-tests/bin" >>= embedDir)

benchmarks :: [Suite]
benchmarks =
  [ suite "coremark" (SNat @14) 20_000_000 fromCoremark $(makeRelativeToProject "programs/coremark/bin" >>= embedDir)
  ]

-- | riscv-tests reports through @gp@, which it uses as @TESTNUM@.
fromTestnum :: RegFile -> Maybe String
fromTestnum regs
  | gp == 1 = Nothing
  | otherwise = Just (printf "failed at test %d (gp = 0x%08x)" (toInteger gp `shiftR` 1) (toInteger gp))
  where
    gp = regs !! (3 :: Index 32)

-- | A C benchmark reports through @a0@, where @programs\/common\/crt0.S@ leaves @main@'s return value.
fromCoremark :: RegFile -> Maybe String
fromCoremark regs = case unpack (regs !! (10 :: Index 32)) :: Signed 64 of
  0 -> Nothing
  -1 -> Just "seeds have no known-good result to validate against"
  -2 -> Just "refused to run"
  n | n > 0 -> Just (printf "%d self-checks failed" (toInteger n))
  n -> Just (printf "returned %d" (toInteger n))
