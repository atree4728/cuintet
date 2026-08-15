-- | The riscv-tests ISA suites as a tasty tree.  The images and the verdict they report live in "Programs".
module Tests.Cuintet.RiscvTests (tests) where

import Programs (Outcome, Suite (..), failure, riscvTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Prelude

tests :: TestTree
tests = testGroup riscvTests.name [testCase name (assertPassed outcome) | (name, outcome) <- riscvTests.outcomes]

assertPassed :: Outcome -> IO ()
assertPassed = maybe (pure ()) assertFailure . failure
