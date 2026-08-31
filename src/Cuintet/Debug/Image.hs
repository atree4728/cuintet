-- | The memory image a core boots from, built from whatever holds the program.
module Cuintet.Debug.Image (Image, instImage, binImage, elfImage) where

import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)
import Control.Exception (bracket)
import Cuintet.Eei (Inst, MemDataBytes)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.IO (hClose, openBinaryTempFile)
import System.Process (callProcess)
import Text.Printf (printf)
import Prelude qualified as P

-- | The whole of the core's memory, @2 ^ ramAddrWidth@ bus words of it.
type Image ramAddrWidth = Vec (2 ^ ramAddrWidth) (BitVector (MemDataBytes * 8))

nop :: Inst
nop = 0x00000013

packInsts :: [Inst] -> [BitVector (MemDataBytes * 8)]
packInsts [] = []
packInsts [_] = error "packInsts: odd number of instructions"
packInsts (l : h : rest) = h ++# l : packInsts rest

-- | The instructions padded out to the whole RAM.
instImage :: forall w. (KnownNat w) => SNat w -> [Inst] -> Image w
instImage SNat ws
  | P.length ws > maxInsts = error (printf "image: %d instructions, only %d fit" (P.length ws) maxInsts)
  | otherwise = unsafeFromList (packInsts $ P.take maxInsts (ws <> P.repeat nop))
  where
    maxInsts = 2 * natToNum @(2 ^ w)

-- | The image read straight from the flat bytes @objcopy -O binary@ writes.
binImage :: (KnownNat w) => SNat w -> BS.ByteString -> Image w
binImage w = instImage w . words32 . pad
  where
    pad bs = bs <> BS.replicate ((4 - BS.length bs `mod` 4) `mod` 4) 0
    words32 bs
      | BS.null bs = []
      | otherwise = let (x, rest) = BS.splitAt 4 bs in little x : words32 rest
    little = BS.foldr (\b acc -> acc * 256 + fromIntegral b) 0

-- | The image an ELF loads, as @objcopy -O binary@ lays it out.
elfImage :: (KnownNat w) => SNat w -> FilePath -> IO (Image w)
elfImage w elf = do
  objcopy <- (<> "objcopy") . fromMaybe "riscv64-unknown-elf-" <$> lookupEnv "RISCV_PREFIX"
  tmp <- getTemporaryDirectory
  bracket (openBinaryTempFile tmp "cuintet.bin") (removeFile . fst) $ \(path, h) -> do
    hClose h
    callProcess objcopy ["-O", "binary", elf, path]
    binImage w <$> BS.readFile path
