{- | The programs under @programs\/@, and what running each one came to.

Every image reports itself the same way -- leave a result in a register, execute
@ecall@ -- so a suite is just a directory of images plus the size of memory they
were linked for and the register their verdict lives in.  'Cuintet.Debug.Sim'
handles the rest.
-}
module Programs (Suite (..), Outcome (..), Stats (..), ipc, failure, riscvTests, benchmarks) where

import Clash.Prelude
import Cuintet.Debug.Sim (Run (..), hexProgram, runImage)
import Cuintet.Eei (RegFile)
import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (sortOn)
import System.FilePath (takeBaseName)
import Text.Printf (printf)

-- | A directory of images, already run.
data Suite = Suite
  { name :: String
  , outcomes :: [(String, Outcome)]
  -- ^ One per image, by base name, in a stable order.
  }

data Stats = Stats
  { cycles :: Int
  , retired :: Int
  }

ipc :: Stats -> Double
ipc s = fromIntegral s.retired / fromIntegral s.cycles

-- | What running one image came to.
data Outcome
  = -- | Reached its @ecall@ and passed.
    Ok Stats
  | -- | Reached its @ecall@, but the verdict rejected it.
    Failed Stats String
  | -- | Never reached its @ecall@ within the cycle budget.
    Hung String

-- | Why the image did not pass, if it did not.
failure :: Outcome -> Maybe String
failure (Ok _) = Nothing
failure (Failed _ err) = Just err
failure (Hung err) = Just err

{- | Runs every image in a directory.  Takes the memory size as an 'SNat' so the
suites below can each name their own without the type escaping.
-}
suite ::
  (KnownNat ramAddrWidth) =>
  -- | as it appears in the test tree and the bench table
  String ->
  -- | bus words of memory the images were linked for
  SNat ramAddrWidth ->
  -- | cycles to simulate before reporting an image as hung
  Int ->
  -- | reads the verdict out of the register file the image left behind
  (RegFile -> Either String ()) ->
  -- | the images, as 'embedDir' produced them
  [(FilePath, BC.ByteString)] ->
  Suite
suite name ramAddrWidth budget verdict images =
  Suite {name, outcomes = [(takeBaseName path, run path bs) | (path, bs) <- sortOn fst images]}
  where
    run path bs = case runImage budget (hexProgram ramAddrWidth path (BC.unpack bs)) of
      Left err -> Hung err
      Right r -> either (Failed (stats r)) (const (Ok (stats r))) (verdict r.regs)
      where
        stats r = Stats {cycles = r.cycles, retired = r.retired}

riscvTests :: Suite
riscvTests =
  suite "riscv-tests" (SNat @10) 200_000 fromTestnum $(makeRelativeToProject "programs/riscv-tests/hex" >>= embedDir)

-- | The benchmarks, which report a cycle count as well as a verdict.
benchmarks :: [Suite]
benchmarks =
  [ suite "coremark" (SNat @14) 20_000_000 fromCoremark $(makeRelativeToProject "programs/coremark/hex" >>= embedDir)
  ]

{- | riscv-tests reports through @gp@, which it uses as @TESTNUM@: 1 once a test
has passed, and @failing testnum \<\< 1 .|. 1@ once one has failed.  See
@programs\/riscv-tests\/env\/riscv_test.h@.
-}
fromTestnum :: RegFile -> Either String ()
fromTestnum regs
  | gp == 1 = Right ()
  | otherwise = Left (printf "failed at test %d (gp = 0x%08x)" (toInteger gp `shiftR` 1) (toInteger gp))
  where
    gp = regs !! (3 :: Index 32)

{- | A C benchmark reports through @a0@, where @programs\/common\/crt0.S@ leaves
@main@'s return value.

For CoreMark that is @total_errors@, which @gen.sh@ rewrites @main@ to return:
zero once the three CRCs and the data-type check have all passed, one per check
that failed, -1 if the seeds have no known-good result to compare against, and
-2 if it refused to run at all.
-}
fromCoremark :: RegFile -> Either String ()
fromCoremark regs = case unpack (regs !! (10 :: Index 32)) :: Signed 64 of
  0 -> Right ()
  -1 -> Left "seeds have no known-good result to validate against"
  -2 -> Left "refused to run"
  n | n > 0 -> Left (printf "%d self-checks failed" (toInteger n))
  n -> Left (printf "returned %d" (toInteger n))
