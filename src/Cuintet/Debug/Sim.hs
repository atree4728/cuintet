-- | Running a bare-metal image on the core in simulation.
module Cuintet.Debug.Sim (Run (..), ipc, isEcall, traceImage, upToEcall, retires, finalRegs, runImage) where

import Clash.Prelude
import Cuintet (system)
import Cuintet.Core (CoreOut (..), CoreTrace (..))
import Cuintet.Debug.Image (Image)
import Cuintet.Eei (RegFile, pattern ENVIRONMENT_CALL_FROM_M_MODE)
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.Ram (initRamLanes)
import Data.Maybe (catMaybes)
import Prelude qualified as P

-- | What an image left behind when it stopped.
data Run = Run
  { cycles :: Int
  , retired :: Int
  , regs :: RegFile
  , halted :: Bool
  }

ipc :: Run -> Double
ipc Run {..} = fromIntegral retired / fromIntegral cycles

-- | Whether a 'Retire' is the @ecall@ that halts an image.
isEcall :: Retire -> Bool
isEcall Retire {trap}
  | Just ENVIRONMENT_CALL_FROM_M_MODE <- trap = True
  | otherwise = False

-- | Every clock the core ran, up to @budget@ of them.
traceImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> [CoreTrace]
traceImage budget img = sampleWithResetN @System d1 budget $ (.trace) <$> system (initRamLanes img)

-- | The trace cut short at the @ecall@ that halts an image, which it keeps.
upToEcall :: [CoreTrace] -> [CoreTrace]
upToEcall = P.foldr (\t rest -> t : if containEcall t then [] else rest) []
  where
    containEcall tr = any (maybe False isEcall) tr.retired

retires :: [CoreTrace] -> [Retire]
retires = P.concatMap (catMaybes . toList . (.retired))

-- | The register file a run of 'Retire's leaves behind.
finalRegs :: [Retire] -> RegFile
finalRegs = P.foldl' writeBack (replicate d32 0)

writeBack :: RegFile -> Retire -> RegFile
writeBack regs l = maybe regs (\(a, v) -> replace a v regs) l.rd

-- | Runs an image to its @ecall@, summarising the trace as it streams past.
runImage :: (KnownNat ramAddrWidth) => Int -> Image ramAddrWidth -> Run
runImage budget = P.foldl' step initial . upToEcall . traceImage budget
  where
    initial = Run {cycles = 0, retired = 0, regs = replicate d32 0, halted = False}
    step :: Run -> CoreTrace -> Run
    step run tr = P.foldl' commit run {cycles = run.cycles + 1} (catMaybes (toList tr.retired))
      where
        commit r retire = run {retired = r.retired + 1, regs = writeBack r.regs retire, halted = isEcall retire}
