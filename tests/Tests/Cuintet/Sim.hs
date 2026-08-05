module Tests.Cuintet.Sim (memImage, runProgram, finalRegs) where

import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)
import Cuintet (system)
import Cuintet.Core (InstLog (..))
import Cuintet.Eei (Inst, XLen)
import Cuintet.Memory (initRamLanes)
import Data.Maybe (catMaybes)
import qualified Prelude as P

-- | canonical NOP (@addi x0, x0, 0@) in RISC-V.
nop :: Inst
nop = 0x00000013

memImage :: [Inst] -> Vec 256 Inst
memImage prog = unsafeFromList (P.take 256 (prog P.++ P.repeat nop))

runProgram :: Int -> [Inst] -> [InstLog]
runProgram n prog =
  P.take n (catMaybes (sampleN @System (16 + 8 * n) (system (initRamLanes (memImage prog)))))

finalRegs :: Int -> [Inst] -> Vec 32 (BitVector XLen)
finalRegs n prog = P.foldl apply (replicate d32 0) (runProgram n prog)
  where
    apply regs l = maybe regs (\(a, d) -> replace a d regs) l.wbReq
