{- | Running a bare-metal image on the core in simulation.

Every image the project runs -- the riscv-tests suites, CoreMark, anything added
beside them -- reports itself the same way: it leaves a result in a register and
executes @ecall@, and the simulation stops there.  'runImage' is that protocol
and nothing more.  It returns the cycle count and the register file; which
register carries the verdict, and what counts as a pass, is the caller's
business.
-}
module Cuintet.Debug.Sim (Image, Run (..), hexProgram, runImage, traceImage) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..), CoreTrace (..))
import Cuintet.CoreCtrl (InstCtrl (systemOp))
import Cuintet.Debug.Image (hexImage)
import Cuintet.Eei (MemDataBytes, RegFile, SystemOp (..))
import Cuintet.Pipeline (MaWb (..), destReg)
import Cuintet.Unit.Ram (initRamLanes)
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

{- | Parses a hex listing into an image sized by the given RAM width.  Takes the
width as an argument so a caller holding one existentially can still use it.
-}
hexProgram :: (KnownNat ramAddrWidth) => SNat ramAddrWidth -> FilePath -> String -> Image ramAddrWidth
hexProgram SNat = hexImage

-- | Whether a retired instruction is the @ecall@ an image halts on.
isEcall :: MaWb -> Bool
isEcall l
  | Just SysEcall <- l.ctrl.systemOp = True
  | otherwise = False

{- | Simulates until the @ecall@, or gives up after @budget@ cycles and reports
the image as hung.  A trap counts as hanging: every environment here points
@mtvec@ at a spin, so an unexpected trap runs out the budget rather than
restarting at @_start@ through a zero @mtvec@.

The core has no architectural register file to read out, so this rebuilds one
from 'Cuintet.Core.instLog', which carries every write-back in the clock it
retires.
-}
runImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> Either String Run
runImage budget img = go 0 0 (replicate d32 0) instLogs
  where
    instLogs = sampleWithResetN @System d1 budget $ (.instLog) <$> system (initRamLanes img)

    go :: Int -> Int -> RegFile -> [Maybe MaWb] -> Either String Run
    go _ _ _ [] = Left (printf "no ecall within %d cycles" budget)
    go !n !r regs (entry : rest) = case entry of
      Just l | isEcall l -> Right Run {cycles = n, retired = r, regs}
      Just l -> go (n + 1) (r + 1) (maybe regs (\a -> replace a l.wbData regs) (destReg l)) rest
      Nothing -> go (n + 1) r regs rest

{- | The per-cycle trace up to and including the @ecall@, for
'Cuintet.Debug.Konata.konataLog'.  Runs to @budget@ cycles if no @ecall@ comes,
rather than failing: a partial trace is still worth looking at.
-}
traceImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> [CoreTrace]
traceImage budget img = upToEcall $ sampleWithResetN @System d1 budget $ (.trace) <$> system (initRamLanes img)
  where
    upToEcall = P.foldr (\t rest -> t : if maybe False isEcall t.instLog then [] else rest) []
