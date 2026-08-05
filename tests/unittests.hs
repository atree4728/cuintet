module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Cuintet.Core
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [Tests.Cuintet.Core.tests]
