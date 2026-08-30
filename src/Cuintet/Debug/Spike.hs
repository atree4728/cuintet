-- | Reading spike's @--log-commits@ output, and diffing it against the core's.
module Cuintet.Debug.Spike (commits, diverged, divergenceLines, withCommits) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Debug.Show (retireLines)
import Cuintet.Eei (Addr, BusReq (..), MemReq, Width (..), XLen, laneOffset, resetVector)
import Cuintet.Pipeline (Retire (..))
import Cuintet.Unit.LoadStore (storeLanes)
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Numeric (readHex)
import System.Exit (die)
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (CreatePipe), proc, withCreateProcess)
import Text.Printf (printf)
import Prelude qualified as P

-- | The 'Retire's in a spike commit log.
commits :: String -> [Retire]
commits = mapMaybe commit . P.lines

commit :: String -> Maybe Retire
commit line = case P.words line of
  "core" : _hart : _priv : pc : inst : rest -> do
    addr <- hex pc
    guard (addr >= toInteger resetVector)
    instBits <- hex (P.filter (`P.notElem` "()") inst)
    fields
      Retire
        { pc = fromInteger addr
        , instBits = fromInteger instBits
        , rd = Nothing
        , mem = Nothing
        , trap = Nothing
        }
      rest
  _ -> Nothing

-- | The register, CSR and memory tokens trailing a commit line.
fields :: Retire -> [String] -> Maybe Retire
fields l [] = Just l
fields l ("mem" : addr : rest) = do
  a <- hex addr
  access <- case rest of
    [] -> Just BusReq {addr = fromInteger a, wdata = Nothing}
    [value] -> stored (fromInteger a) value
    _ -> Nothing
  fields l {mem = Just access} []
fields l (name : value : rest)
  | Just reg <- regNumber name, Just v <- hex value = fields l {rd = Just (fromInteger reg, fromInteger v)} rest
  | 'c' : _ <- name, Just _ <- hex value = fields l rest
fields _ _ = Nothing

-- | A store, whose width spike gives as the number of digits it padded to.
stored :: Addr -> String -> Maybe MemReq
stored addr value = do
  digits <- stripPrefix "0x" value
  width <- case P.length digits of
    2 -> Just Byte
    4 -> Just Half
    8 -> Just Word
    16 -> Just Double
    _ -> Nothing
  v <- hex value
  pure BusReq {addr, wdata = Just (storeLanes width (laneOffset addr) (fromInteger v :: BitVector XLen))}

-- | The register number in an @x7@ token.
regNumber :: String -> Maybe Integer
regNumber ('x' : ds) = case reads ds of
  [(n, "")] -> Just n
  _ -> Nothing
regNumber _ = Nothing

hex :: String -> Maybe Integer
hex s = do
  digits <- stripPrefix "0x" s
  case readHex digits of
    [(v, "")] -> Just v
    _ -> Nothing

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
    <> foldMap indent (P.drop (i - context) (P.take i ours))
  where
    side = maybe ["    (the trace ends here)"] indent
    indent = P.map ("    " <>) . retireLines

withCommits :: FilePath -> ([Retire] -> IO r) -> IO r
withCommits elf k =
  withCreateProcess (proc "spike" args) {std_err = CreatePipe} $ \_ _ err _ ->
    case err of
      Nothing -> die "spike: could not open a pipe to its log"
      Just h -> k . commits =<< hGetContents h
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
