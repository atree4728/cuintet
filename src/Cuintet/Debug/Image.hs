-- | The memory image a core boots from, built from whatever holds the program.
module Cuintet.Debug.Image (Image, instImage, binImage, elfImage) where

import Clash.Prelude
import Cuintet.Eei (Inst, MemDataBytes)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)
import Text.Printf (printf)
import Prelude qualified as P

-- | The whole of the core's memory, @2 ^ ramAddrWidth@ bus words of it.
type Image ramAddrWidth = Vec (2 ^ ramAddrWidth) (BitVector (MemDataBytes * 8))

nop :: Inst
nop = 0x00000013

-- | The instructions padded out to the whole RAM.
instImage :: forall w. (KnownNat w) => SNat w -> [Inst] -> Image w
instImage SNat ws
  | P.length ws > maxInsts = error (printf "image: %d instructions, only %d fit" (P.length ws) maxInsts)
  | otherwise = unfoldr (SNat @(2 ^ w)) packer ws
  where
    maxInsts = 2 * natToNum @(2 ^ w)
    packer [] = (nop ++# nop, [])
    packer [l] = (nop ++# l, [])
    packer (l : h : rest) = (h ++# l, rest)

-- | The image read straight from the flat bytes @objcopy -O binary@ writes.
binImage :: (KnownNat w) => SNat w -> BS.ByteString -> Image w
binImage w = instImage w . toWords
  where
    toWord = pack . sum . P.zipWith (\i c -> (fromIntegral c :: Word32) `shiftL` (i * 8)) [0 ..] . BS.unpack
    toWords bs
      | BS.null bs = []
      | otherwise = let (chunk, rest) = BS.splitAt 4 bs in toWord chunk : toWords rest

-- | The image an ELF loads, as @objcopy -O binary@ lays it out.
elfImage :: (KnownNat w) => SNat w -> FilePath -> IO (Image w)
elfImage w elf = withSystemTempFile "cuintet.bin" $ \path h -> do
  hClose h
  objcopy <- (<> "objcopy") . fromMaybe "riscv64-unknown-elf-" <$> lookupEnv "RISCV_PREFIX"
  callProcess objcopy ["-O", "binary", elf, path]
  binImage w <$> BS.readFile path
