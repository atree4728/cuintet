module Main (main) where

import Clash.Prelude
import Cuintet.Debug.Konata (konataLog)
import Cuintet.Debug.Sim (hexProgram, traceImage)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), die)
import System.FilePath (takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Prelude qualified as P

ramAddrWidth :: SNat 16
ramAddrWidth = SNat

budget :: Int
budget = 400000

outDir :: FilePath
outDir = "build" </> "konata"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> run input
    _ -> die "usage: konata IMAGE.hex"

run :: FilePath -> IO ()
run input = do
  rev <- gitRev
  let output = outDir </> takeBaseName input <> "-" <> rev <> ".kanata.log"
  createDirectoryIfMissing True outDir
  img <- hexProgram ramAddrWidth input <$> P.readFile input
  P.writeFile output $ P.unlines $ konataLog $ traceImage budget img
  hPutStrLn stderr (output <> ": written")

gitRev :: IO String
gitRev = do
  rev <- git ["rev-parse", "--short", "HEAD"]
  dirty <- git ["status", "--porcelain"]
  pure $ case (rev, dirty) of
    (Just (r : _), Just []) -> r
    (Just (r : _), _) -> r <> "-dirty"
    _ -> "unknown"
  where
    git args = do
      (code, out, _) <- readProcessWithExitCode "git" args ""
      pure $ if code == ExitSuccess then Just (P.lines out) else Nothing
