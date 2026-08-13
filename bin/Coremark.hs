{- | Run a bare-metal image on the core until it halts.

The image is built by @bench\/coremark\/gen.sh@, whose @crt0.S@ executes @ecall@
once @main@ has returned.  Reaching that @ecall@ is what says the benchmark ran
to completion; @a0@ holds whatever @main@ returned.
-}
module Main (main) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..))
import Cuintet.Eei (Inst, MemDataBytes, RegAddr, XLen)
import Cuintet.Debug.Image (hexImage)
import Cuintet.Pipeline (MaWb (..), destReg)
import Cuintet.Unit.Ram (initRamLanes)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)
import Prelude qualified as P

-- | Bus words of memory, matching @RAM_SIZE@ in @bench\/coremark\/link.ld@.
type RamAddrWidth = 14

-- | Cycles to simulate before reporting the image as hung.
maxCycles :: Int
maxCycles = 20_000_000

-- | @ecall@, on which @crt0@ halts.
ecall :: Inst
ecall = 0x00000073

-- | @a0@, which holds @main@'s return value.
a0 :: RegAddr
a0 = 10

main :: IO ()
main = do
  args <- getArgs
  path <- case args of
    [] -> pure "bench/coremark/coremark.hex"
    [path] -> pure path
    _ -> die "usage: coremark [IMAGE.hex]"
  img <- hexImage path <$> P.readFile path
  case run img of
    Left err -> die (path <> ": " <> err)
    Right (cycles, ret) -> do
      printf "%s: halted after %d cycles\n" path cycles
      printf "a0 = %d (0x%x)\n" (toInteger ret) (toInteger ret)

{- | Simulates until the @ecall@, rebuilding the register file from the
write-backs the core logs, and reads @a0@ out of it.
-}
run :: Vec (2 ^ RamAddrWidth) (BitVector (MemDataBytes * 8)) -> Either String (Int, BitVector XLen)
run img = go 0 (replicate d32 0) instLogs
  where
    instLogs = sampleN @System maxCycles $ (.instLog) <$> system (initRamLanes img)

    go :: Int -> Vec 32 (BitVector XLen) -> [Maybe MaWb] -> Either String (Int, BitVector XLen)
    go _ _ [] = Left (printf "no ecall within %d cycles" maxCycles)
    go !n regs (entry : rest) = case entry of
      Just l | l.instBits == ecall -> Right (n, regs !! a0)
      Just l -> go (n + 1) (maybe regs (\a -> replace a l.wbData regs) (destReg l)) rest
      Nothing -> go (n + 1) regs rest
