module Main (main) where

import Clash.Prelude
import Control.Exception (bracket)
import Cuintet.Debug.Show (retireLines)
import Cuintet.Debug.Sim (hexProgram, retireImage)
import Cuintet.Debug.Spike (commits, diverged)
import Cuintet.Eei (pattern ENVIRONMENT_CALL_FROM_M_MODE)
import Cuintet.Pipeline (Retire (..))
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isNothing)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hClose, hGetContents, openBinaryTempFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), callProcess, proc, withCreateProcess)
import Text.Printf (printf)
import Prelude qualified as P

ramAddrWidth :: SNat 14
ramAddrWidth = SNat

budget :: Int
budget = 20_000_000

-- | Retires to print either side of a mismatch.
context :: Int
context = 3

main :: IO ()
main = do
  args <- getArgs
  case args of
    [elf] -> run elf
    _ -> die "usage: tracediff IMAGE.elf"

run :: FilePath -> IO ()
run elf = do
  img <- hexProgram ramAddrWidth elf <$> flatHex elf
  let ours = retireImage budget img
  case P.reverse ours of
    [] -> die (printf "%s: the core retired nothing" elf)
    l : _ | not (isEcall l) -> die (printf "%s: no ecall within %d cycles" elf budget)
    _ -> pure ()
  withSpike elf (report elf (P.filter (isNothing . (.trap)) ours))

-- | Spike logs no line for an instruction that trapped, the halting @ecall@ included.
isEcall :: Retire -> Bool
isEcall l
  | Just ENVIRONMENT_CALL_FROM_M_MODE <- l.trap = True
  | otherwise = False

report :: FilePath -> [Retire] -> [Retire] -> IO ()
report elf ours theirs = case diverged ours theirs of
  Nothing -> printf "%s: %d retires match spike\n" elf (P.length ours)
  Just (i, ourEntry, theirEntry) -> do
    printf "%s: mismatch at retire %d\n\n" elf i
    putStrLn "  cuintet:"
    side ourEntry
    putStrLn "  spike:"
    side theirEntry
    printf "\n  the %d retires before it:\n" context
    mapM_ (mapM_ (putStrLn . ("    " <>)) . retireLines) (P.drop (i - context) (P.take i ours))
    printf "\nRetire %d is the number the Konata log's R lines carry.\n" i
    exitFailure
  where
    side = maybe (putStrLn "    (the trace ends here)") (mapM_ (putStrLn . ("    " <>)) . retireLines)

{- | The ELF's flat image as the hex listing 'hexProgram' parses: one 32-bit
little-endian word per line, low address first, the same as
@programs\/common\/hex.sh@ produces.  Deriving it here rather than reading a
checked-in @.hex@ is what guarantees the core and spike run the same program.
-}
flatHex :: FilePath -> IO String
flatHex elf = do
  objcopy <- (<> "objcopy") <$> prefix
  tmp <- getTemporaryDirectory
  bracket (openBinaryTempFile tmp "cuintet.bin") (removeFile . fst) $ \(path, h) -> do
    hClose h
    callProcess objcopy ["-O", "binary", elf, path]
    bin <- BS.readFile path
    pure (P.unlines (P.map word (words32 (pad bin))))
  where
    pad bs = bs <> BS.replicate ((4 - BS.length bs `mod` 4) `mod` 4) 0
    word = printf "%08x" . BS.foldr (\b acc -> acc * 256 + toInteger b) 0
    words32 bs
      | BS.null bs = []
      | otherwise = let (w, rest) = BS.splitAt 4 bs in w : words32 rest

{- | Runs spike on the same ELF and hands its commit log to @k@ as it arrives.

Reading it from a pipe rather than a file is what bounds the run: spike is
killed once @k@ has seen as much as it needs, so an image that never halts --
CoreMark, whose @ecall@ traps into a handler that spins -- costs only the log
the comparison actually reads.
-}
withSpike :: FilePath -> ([Retire] -> IO r) -> IO r
withSpike elf k =
  withCreateProcess (proc "spike" args) {std_err = CreatePipe} $ \_ _ err _ ->
    case err of
      Nothing -> die "tracediff: could not open a pipe to spike"
      Just h -> k . commits =<< hGetContents h
  where
    args =
      [ "--isa=rv64im_zicsr_zicntr"
      , "-m0x80000000:0x20000"
      , "--priv=m"
      , -- the images probe for optional CSRs and skip past the ones that trap,
        -- so spike has to be cut down to the CSRs the core implements
        "--pmpregions=0"
      , "--log-commits"
      , elf
      ]

prefix :: IO String
prefix = fromMaybe "riscv64-unknown-elf-" <$> lookupEnv "RISCV_PREFIX"
