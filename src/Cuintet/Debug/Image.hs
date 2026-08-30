module Cuintet.Debug.Image (memImage, hexImage, binImage, elfImage) where

import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)
import Control.Exception (bracket)
import Cuintet.Eei (Inst, XLen)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Numeric (readHex)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.IO (hClose, openBinaryTempFile)
import System.Process (callProcess)
import Text.Printf (printf)
import Prelude qualified as P

nop :: Inst
nop = 0x00000013

packInsts :: [Inst] -> [BitVector XLen]
packInsts [] = []
packInsts [_] = error "packInsts: odd number of instructions"
packInsts (l : h : rest) = h ++# l : packInsts rest

-- | The instructions padded out to the whole RAM.
image :: forall n. (KnownNat n) => FilePath -> [Inst] -> Vec n (BitVector XLen)
image path ws
  | P.length ws > maxInsts = error (printf "%s: RAM size is insufficient" path)
  | otherwise = unsafeFromList (packInsts $ P.take maxInsts (ws <> P.repeat nop))
  where
    maxInsts = 2 * natToNum @n

memImage :: [Inst] -> Vec 128 (BitVector XLen)
memImage = image "<program>"

hexImage :: (KnownNat n) => FilePath -> String -> Vec n (BitVector XLen)
hexImage path src = image path (P.zipWith parseWord [1 :: Int ..] (P.lines src))
  where
    parseWord lineNo s = case readHex s of
      [(w, "")] -> w
      _ -> error (printf "%s:%d: not a hex word: %s" path lineNo s)

-- | The image read straight from the flat bytes @objcopy -O binary@ writes.
binImage :: (KnownNat n) => FilePath -> BS.ByteString -> Vec n (BitVector XLen)
binImage path = image path . words32 . pad
  where
    pad bs = bs <> BS.replicate ((4 - BS.length bs `mod` 4) `mod` 4) 0
    words32 bs
      | BS.null bs = []
      | otherwise = let (w, rest) = BS.splitAt 4 bs in little w : words32 rest
    little = BS.foldr (\b acc -> acc * 256 + fromIntegral b) 0

-- | The image an ELF loads, as @objcopy -O binary@ lays it out.
elfImage :: (KnownNat n) => FilePath -> IO (Vec n (BitVector XLen))
elfImage elf = do
  objcopy <- (<> "objcopy") . fromMaybe "riscv64-unknown-elf-" <$> lookupEnv "RISCV_PREFIX"
  tmp <- getTemporaryDirectory
  bracket (openBinaryTempFile tmp "cuintet.bin") (removeFile . fst) $ \(path, h) -> do
    hClose h
    callProcess objcopy ["-O", "binary", elf, path]
    binImage elf <$> BS.readFile path
