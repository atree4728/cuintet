{- | The riscv-tests ISA suite, run against the core.

Each image is assembled from @vendor/riscv-tests@ by @tests\/riscv-tests\/gen.sh@
and embedded at compile time, so running these needs no RISC-V toolchain.  A
test reports its verdict by leaving it in @gp@ and executing @ecall@; the
simulation stops there.
-}
module Tests.Cuintet.RiscvTests (tests) where

import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)
import Cuintet (system)
import Cuintet.Core (InstLog (..))
import Cuintet.Eei (Inst, XLen)
import Cuintet.Memory (initRamLanes)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Numeric (readHex)
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Printf (printf)
import qualified Prelude as P

{- | Words of memory the core is given.  The images are linked at
@0x80000000@, which 'Cuintet.Eei.toWordAddr' truncates to word zero, so an
image loaded at offset zero is addressed correctly by both @lui@/@addi@ and
@auipc@.  The largest of them is under a thousand words.
-}
type MemWords = 2048

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

-- | Decodes an image, zero-padded to the size of the memory.
image :: FilePath -> BC.ByteString -> Vec MemWords Inst
image path bs
  | P.length ws > depth = errorWithoutStackTrace (printf "%s: %d words do not fit in %d" path (P.length ws) depth)
  | otherwise = unsafeFromList (P.take depth (ws P.++ P.repeat 0))
  where
    ws = P.zipWith (parseWord path) [1 ..] (P.lines (BC.unpack bs))
    depth = natToNum @MemWords

parseWord :: FilePath -> Int -> String -> Inst
parseWord path lineNo s = case readHex s of
  [(w, "")] -> fromInteger (w :: Integer)
  _ -> errorWithoutStackTrace (printf "%s:%d: not a hex word: %s" path lineNo s)

{- | Runs an image until its @ecall@, rebuilding the register file from the
write-backs the core logs, and reads the verdict out of 'testnum'.
-}
runImage :: Vec MemWords Inst -> Either String ()
runImage img = verdict (replicate d32 0) instLogs
  where
    instLogs = catMaybes (sampleN @System maxCycles (system (initRamLanes img)))

    verdict :: Vec 32 (BitVector XLen) -> [InstLog] -> Either String ()
    verdict _ [] = Left (printf "no ecall within %d cycles" maxCycles)
    verdict regs (l : ls)
      | l.inst == ecall = report (regs !! testnum)
      | otherwise = verdict (maybe regs (\(a, d) -> replace a d regs) l.wbReq) ls

    report gp
      | gp == 1 = Right ()
      | otherwise = Left (printf "failed at test %d (gp = 0x%08x)" (toInteger gp `shiftR` 1) (toInteger gp))

tests :: TestTree
tests =
  testGroup
    "riscv-tests"
    [ testCase (takeBaseName path) (either assertFailure pure (runImage (image path bs)))
    | (path, bs) <- sortOn fst images
    ]
