-- | The riscv-tests ISA suites as a tasty tree.  The images and the verdict they report live in "Programs".
module Tests.Cuintet.RiscvTests (tests) where

import Programs (Outcome (..), Suite (..), riscvTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Prelude

tests :: TestTree
tests = testGroup riscvTests.name [testCase o.name (assertPassed o) | o <- riscvTests.outcomes]

assertPassed :: Outcome -> IO ()
assertPassed = maybe (pure ()) assertFailure . (.failure)
