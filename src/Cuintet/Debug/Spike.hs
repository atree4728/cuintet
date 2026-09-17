{-# LANGUAGE OverloadedStrings #-}

-- | Reading spike's @--log-commits@ output, and diffing it against the core's.
module Cuintet.Debug.Spike (commits, diverged, divergenceLines, withCommits) where

import Clash.Prelude (BitVector)
import Control.Applicative (empty, (<|>))
import Control.Monad (guard)
import Cuintet.Debug.Show (retireLines)
import Cuintet.Eei (Addr, BusReq (..), MemReq, RegAddr, Width (..), XLen, laneOffset, resetVector, storeLanes)
import Cuintet.Pipeline (Retire (..))
import Data.Attoparsec.ByteString (match)
import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfInput, hexadecimal, many', option, parseOnly, skipSpace, skipWhile, string)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Char (isSpace)
import Data.Maybe (listToMaybe, mapMaybe)
import System.Exit (die)
import System.Process (CreateProcess (..), StdStream (CreatePipe), proc, withCreateProcess)
import Text.Printf (printf)
import Prelude

-- | The 'Retire's in a spike commit log.
commits :: BL.ByteString -> [Retire]
commits = mapMaybe (either (const Nothing) Just . parseOnly line . BL.toStrict) . BL.lines

line :: Parser Retire
line = do
  _hart <- string "core" *> skipSpace *> decimal @Int <* char ':'
  _priv <- skipSpace *> decimal @Int
  pc <- skipSpace *> hex
  guard (pc >= toInteger resetVector)
  instBits <- skipSpace *> char '(' *> hex <* char ')'
  effects <- many' (skipSpace *> effect)
  endOfInput
  pure $ foldr id (bare pc instBits) effects

bare :: Integer -> Integer -> Retire
bare pc instBits =
  Retire {pc = fromInteger pc, instBits = fromInteger instBits, rd = Nothing, mem = Nothing, trap = Nothing}

-- | The register, CSR and memory tokens trailing a commit line.
effect :: Parser (Retire -> Retire)
effect =
  (\a l -> l {mem = Just a}) <$> (string "mem" *> skipSpace *> access)
    <|> (\r l -> l {rd = Just r}) <$> reg
    <|> id <$ csr

reg :: Parser (RegAddr, BitVector XLen)
reg = ((,) . fromInteger <$> (char 'x' *> decimal)) <*> (fromInteger <$> (skipSpace *> hex))

csr :: Parser Integer
csr = char 'c' *> skipWhile (not . isSpace) *> skipSpace *> hex

access :: Parser MemReq
access = do
  addr <- fromInteger <$> hex
  option BusReq {addr, wdata = Nothing} (skipSpace *> stored addr)

stored :: Addr -> Parser MemReq
stored addr = do
  (digits, v) <- string "0x" *> match hexadecimal
  width <- case BS.length digits of
    2 -> pure Byte
    4 -> pure Half
    8 -> pure Word
    16 -> pure Double
    _ -> empty
  pure BusReq {addr, wdata = Just (storeLanes width (laneOffset addr) (fromInteger v :: BitVector XLen))}

hex :: Parser Integer
hex = string "0x" *> hexadecimal

-- | The first retire at which the core's trace departs from spike's.
diverged :: [Retire] -> [Retire] -> Maybe (Int, Maybe Retire, Maybe Retire)
diverged = go 0
  where
    go !i (x : xs) (y : ys)
      | x == y = go (i + 1) xs ys
      | otherwise = Just (i, Just x, Just y)
    go _ [] _ = Nothing
    go i xs [] = Just (i, listToMaybe xs, Nothing)

divergenceLines :: Int -> [Retire] -> (Int, Maybe Retire, Maybe Retire) -> [String]
divergenceLines context ours (i, ourEntry, theirEntry) =
  ["  cuintet:"]
    <> side ourEntry
    <> ["  spike:"]
    <> side theirEntry
    <> ["", printf "  the %d retires before it:" context]
    <> foldMap indent (drop (i - context) (take i ours))
  where
    side = maybe ["    (the trace ends here)"] indent
    indent = map ("    " <>) . retireLines

withCommits :: FilePath -> ([Retire] -> IO r) -> IO r
withCommits elf k =
  withCreateProcess (proc "spike" args) {std_err = CreatePipe} $ \_ _ err _ ->
    case err of
      Nothing -> die "spike: could not open a pipe to its log"
      Just h -> k . commits =<< BL.hGetContents h
  where
    args =
      [ "--isa=rv64im_zicsr_zicntr"
      , printf "-m0x%x:0x20000" (toInteger resetVector)
      , "--priv=m"
      , -- the images skip past CSRs that trap, so cut spike down to the core's
        "--pmpregions=0"
      , "--log-commits"
      , elf
      ]
