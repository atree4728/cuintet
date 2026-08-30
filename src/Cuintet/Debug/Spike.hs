-- | Reading spike's @--log-commits@ output, and diffing it against the core's.
module Cuintet.Debug.Spike (base, commits, diverged, divergenceLines, withCommits) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Debug.Show (retireLines)
import Cuintet.Eei (Addr, BusReq (..), MemReq, Width (..), XLen, laneOffset)
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

-- | Where the images are linked.
base :: Addr
base = 0x80000000

{- | The 'Retire's in a spike commit log.

Lines it cannot read are dropped, which covers spike's warnings as well as the
bootrom it runs before entering the image.

>>> import Prelude
>>> import Cuintet.Debug.Show (retireLines)
>>> logged s = mapM_ (mapM_ putStrLn . retireLines) (commits s)
>>> logged "core   0: 3 0x0000000080000038 (0x0002b383) x7  0x1122334455667788 mem 0x0000000080000100"
00000038 : 0002b383
  reg[ 7] <= 1122334455667788
  mem[00000100] load

A store reports the bytes it wrote, padded to its width:

>>> logged "core   0: 3 0x0000000080000034 (0x00628823) mem 0x0000000080000110 0x88"
00000034 : 00628823
  mem[00000110] <= --------------88

A CSR write is not a 'Retire' field, so the line keeps only its pc:

>>> logged "core   0: 3 0x0000000080000040 (0x30529073) c773_mtvec 0x0000000080000100"
00000040 : 30529073
-}
commits :: String -> [Retire]
commits = mapMaybe commit . P.lines

commit :: String -> Maybe Retire
commit line = case P.words line of
  "core" : _hart : _priv : pc : inst : rest -> do
    addr <- hex pc
    guard (addr >= toInteger base)
    instBits <- hex (P.filter (`P.notElem` "()") inst)
    fields
      Retire
        { pc = fromInteger (addr - toInteger base)
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
  a <- subtract (toInteger base) <$> hex addr
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

{- | The first retire at which the core's trace departs from spike's.

The core's is the shorter of the two by construction: it stops at the @ecall@
that halts the image, where spike goes on to run the handler behind it.  So the
core running out ends the comparison, and spike running out first is itself a
mismatch, reported as 'Nothing' on its side.

>>> import Prelude
>>> import Cuintet.Pipeline (Retire (..))
>>> l = Retire{pc = 0, instBits = 0x13, rd = Nothing, mem = Nothing, trap = Nothing}
>>> fmap (\(i, ours, theirs) -> (i, (.pc) <$> ours, (.pc) <$> theirs)) (diverged [l, l] [l, l{pc = 4}])
Just (1,Just 0,Just 4)

>>> diverged [l] [l, l] >> Just "mismatch"
Nothing

>>> fmap (\(i, ours, theirs) -> (i, (.pc) <$> ours, (.pc) <$> theirs)) (diverged [l, l] [l])
Just (1,Just 0,Nothing)
-}
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
      , "-m0x80000000:0x20000"
      , "--priv=m"
      , -- the images skip past CSRs that trap, so cut spike down to the core's
        "--pmpregions=0"
      , "--log-commits"
      , elf
      ]
