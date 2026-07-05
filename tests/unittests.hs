module Main where

import Prelude

import Test.Tasty
import qualified Tests.Cuintet.Core

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [Tests.Cuintet.Core.tests]
