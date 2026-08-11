{- | The riscv-tests ISA suite, run against the core.

Each image is assembled from @vendor/riscv-tests@ by @tests\/riscv-tests\/gen.sh@
and embedded at compile time, so running these needs no RISC-V toolchain.  A
test reports its verdict by leaving it in @gp@ and executing @ecall@; the
simulation stops there.
-}
module Tests.Cuintet.RiscvTests (tests) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..))
import Cuintet.Eei (Inst, MemDataBytes)
import Cuintet.Image (hexImage)
import Cuintet.Pipeline (MaWb (..), destReg)
import Cuintet.Unit.Ram (initRamLanes)
import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Printf (printf)

{- | Bus words of memory the core is given.  The images are linked at
@0x80000000@, which 'Cuintet.Memory.memory' truncates to word zero, so an
image loaded at offset zero is addressed correctly by both @lui@/@addi@ and
@auipc@.  The largest of them is under a thousand words.
-}
type RamAddrWidth = 10

-- | Cycles to simulate before reporting a test as hung.
maxCycles :: Int
maxCycles = 200000

-- | @ecall@, on which every test reports its verdict.
ecall :: Inst
ecall = 0x00000073

{- | @gp@, which the suite uses as @TESTNUM@: 1 once a test has passed, and
@failing testnum \<\< 1 .|. 1@ once one has failed.
-}
testnum :: Index 32
testnum = 3

{- | Every image @gen.sh@ has produced, so the suite is exactly what is checked
in.  A hex file added after the last build is only picked up once this module
is recompiled; @cabal clean@ forces that.
-}
images :: [(FilePath, BC.ByteString)]
images = $(makeRelativeToProject "tests/riscv-tests/hex" >>= embedDir)

{- | Runs an image until its @ecall@, rebuilding the register file from the
write-backs the core logs, and reads the verdict out of 'testnum'.
-}
runImage :: Vec (2 ^ RamAddrWidth) (BitVector (MemDataBytes * 8)) -> Either String ()
runImage img = verdict (replicate d32 0) instLogs
  where
    instLogs = catMaybes $ sampleN @System maxCycles $ (.instLog) <$> system (initRamLanes img)

    verdict :: Vec 32 (BitVector (MemDataBytes * 8)) -> [MaWb] -> Either String ()
    verdict _ [] = Left (printf "no ecall within %d cycles" maxCycles)
    verdict regs (l : ls)
      | l.instBits == ecall = report (regs !! testnum)
      | otherwise = verdict (maybe regs (\a -> replace a l.wbData regs) (destReg l)) ls

    report gp
      | gp == 1 = Right ()
      | otherwise = Left (printf "failed at test %d (gp = 0x%08x)" (toInteger gp `shiftR` 1) (toInteger gp))

tests :: TestTree
tests =
  testGroup
    "riscv-tests"
    [ testCase (takeBaseName path) (either assertFailure pure $ runImage $ hexImage path $ BC.unpack bs)
    | (path, bs) <- sortOn fst images
    ]
