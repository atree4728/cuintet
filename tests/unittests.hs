module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Cuintet.Core as Core
import qualified Tests.Cuintet.RiscvTests as RiscvTests
import Prelude

main :: IO ()
main = defaultMain $ testGroup "." [Core.tests, RiscvTests.tests]
