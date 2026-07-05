module Tests.Cuintet.Core where

import Clash.Prelude
import qualified Prelude as P

import Cuintet (system)
import Cuintet.Eei
import Test.Tasty
import Test.Tasty.HUnit

-- | Contents of sample.hex.
sampleHex :: Vec 4 (BitVector ILen)
sampleHex = 0x01234567 :> 0x89abcdef :> 0xdeadbeef :> 0xcafebebe :> Nil

tests :: TestTree
tests =
  testGroup
    "Cuintet.Core"
    [ testCase "fetches sample.hex in order" $ do
        let fetched = sampleN @System 6 $ system (blockRam sampleHex)
        let memAccess = P.zipWith (P.curry Just) [0, 4, 8, 12] (toList sampleHex)
        fetched @?= Nothing : Nothing : memAccess -- reset, delay, 0, 4, 8, 12
    ]
