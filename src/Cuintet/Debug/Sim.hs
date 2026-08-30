-- | Running a bare-metal image on the core in simulation.
module Cuintet.Debug.Sim (Run (..), isEcall, traceImage, upToEcall, retires, finalRegs, runImage) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..), CoreTrace (..))
import Cuintet.Debug.Image (Image)
import Cuintet.Eei (RegFile, pattern ENVIRONMENT_CALL_FROM_M_MODE)
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.Ram (initRamLanes)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Prelude qualified as P

-- | What an image left behind when it reached its @ecall@.
data Run = Run
  { cycles :: Int
  -- ^ Cycles from the end of reset to the @ecall@.
  , retired :: Int
  , regs :: RegFile
  -- ^ The register file, rebuilt from the write-backs the core logged.
  }

-- | Whether a 'Retire' is the @ecall@ that halts an image.
isEcall :: Retire -> Bool
isEcall l
  | Just ENVIRONMENT_CALL_FROM_M_MODE <- l.trap = True
  | otherwise = False

-- | Every clock the core ran, up to @budget@ of them.  Reset is not among them.
traceImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> [CoreTrace]
traceImage budget img = sampleWithResetN @System d1 budget $ (.trace) <$> system (initRamLanes img)

-- | The trace cut short at the @ecall@ that halts an image, which it keeps.
upToEcall :: [CoreTrace] -> [CoreTrace]
upToEcall = P.foldr (\t rest -> t : if maybe False isEcall t.retired then [] else rest) []

retires :: [CoreTrace] -> [Retire]
retires = mapMaybe (.retired)

-- | The register file a run of 'Retire's leaves behind.
finalRegs :: [Retire] -> RegFile
finalRegs = P.foldl' writeBack (replicate d32 0)

writeBack :: RegFile -> Retire -> RegFile
writeBack regs l = maybe regs (\(a, v) -> replace a v regs) l.rd

-- | Runs an image to its @ecall@, summarising it as it goes.
runImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> Either String Run
runImage budget img = go 0 0 (replicate d32 0) (traceImage budget img)
  where
    go :: Int -> Int -> RegFile -> [CoreTrace] -> Either String Run
    go _ _ _ [] = Left (printf "no ecall within %d cycles" budget)
    go !n !r regs (t : rest) = case t.retired of
      Just l | isEcall l -> Right Run {cycles = n, retired = r, regs}
      Just l -> go (n + 1) (r + 1) (writeBack regs l) rest
      Nothing -> go (n + 1) r regs rest
