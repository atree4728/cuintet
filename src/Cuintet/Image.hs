module Cuintet.Image (packInsts, memImage, hexImage) where

import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)
import Cuintet.Eei (Inst, XLen)
import Numeric (readHex)
import Text.Printf (printf)
import Prelude qualified as P

-- | canonical NOP (@addi x0, x0, 0@) in RISC-V.
nop :: Inst
nop = 0x00000013

packInsts :: [Inst] -> [BitVector XLen]
packInsts [] = []
packInsts [_] = error "packInsts: odd number of instructions"
packInsts (l : h : rest) = h ++# l : packInsts rest

memImage :: [Inst] -> Vec 128 (BitVector XLen)
memImage prog = unsafeFromList (packInsts $ P.take 256 (prog <> P.repeat nop))

hexImage :: forall n. (KnownNat n) => FilePath -> String -> Vec n (BitVector XLen)
hexImage path src
  | P.length ws > maxInsts = error (printf "%s: RAM size is insufficient" path)
  | otherwise = unsafeFromList (packInsts $ P.take maxInsts (ws <> P.repeat nop))
  where
    maxInsts = 2 * natToNum @n
    ws = P.zipWith parseWord [1 :: Int ..] (P.lines src)
    parseWord lineNo s = case readHex s of
      [(w, "")] -> w
      _ -> error (printf "%s:%d: not a hex word: %s" path lineNo s)
