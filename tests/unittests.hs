module Main where

import Prelude

import Test.Tasty

import qualified Tests.Cuintet.Cpu

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.Cuintet.Cpu.accumTests
      ]
