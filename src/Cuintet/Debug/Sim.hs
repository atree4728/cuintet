{- | Running a bare-metal image on the core in simulation.

Every image the project runs -- the riscv-tests suites, CoreMark, anything added
beside them -- reports itself the same way: it leaves a result in a register and
executes @ecall@, and the simulation stops there.  'runImage' is that protocol
and nothing more.  It returns the cycle count and the register file; which
register carries the verdict, and what counts as a pass, is the caller's
business.
-}
module Cuintet.Debug.Sim (Image, Run (..), hexProgram, runImage, traceImage, retireImage) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..), CoreTrace (..))
import Cuintet.Debug.Image (hexImage)
import Cuintet.Eei (MemDataBytes, RegFile, pattern ENVIRONMENT_CALL_FROM_M_MODE)
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.Ram (initRamLanes)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Prelude qualified as P

-- | The whole of the core's memory, @2 ^ ramAddrWidth@ bus words of it.
type Image ramAddrWidth = Vec (2 ^ ramAddrWidth) (BitVector (MemDataBytes * 8))

-- | What an image left behind when it reached its @ecall@.
data Run = Run
  { cycles :: Int
  -- ^ Cycles from the end of reset to the @ecall@.
  , retired :: Int
  , regs :: RegFile
  -- ^ The register file, rebuilt from the write-backs the core logged.
  }

hexProgram :: (KnownNat ramAddrWidth) => SNat ramAddrWidth -> FilePath -> String -> Image ramAddrWidth
hexProgram SNat = hexImage

isEcall :: Retire -> Bool
isEcall l
  | Just ENVIRONMENT_CALL_FROM_M_MODE <- l.trap = True
  | otherwise = False

runImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> Either String Run
runImage budget img = go 0 0 (replicate d32 0) instLogs
  where
    instLogs = sampleWithResetN @System d1 budget $ (.retired) <$> system (initRamLanes img)

    go :: Int -> Int -> RegFile -> [Maybe Retire] -> Either String Run
    go _ _ _ [] = Left (printf "no ecall within %d cycles" budget)
    go !n !r regs (entry : rest) = case entry of
      Just l | isEcall l -> Right Run {cycles = n, retired = r, regs}
      Just l -> go (n + 1) (r + 1) (maybe regs (\(a, v) -> replace a v regs) l.rd) rest
      Nothing -> go (n + 1) r regs rest

traceImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> [CoreTrace]
traceImage budget img = upToEcall $ sampleWithResetN @System d1 budget $ (.trace) <$> system (initRamLanes img)
  where
    upToEcall = P.foldr (\t rest -> t : if maybe False isEcall t.retired then [] else rest) []

retireImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> [Retire]
retireImage budget = mapMaybe (.retired) . traceImage budget
