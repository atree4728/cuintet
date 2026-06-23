module Tests.Cuintet.Cpu where

import Clash.Hedgehog.Sized.Unsigned (genUnsigned)
import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

import Cuintet.Cpu (accum)

prop_accum :: H.Property
prop_accum = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))
  inp <-
    H.forAll
      ( Gen.list
          (Range.singleton simDuration)
          (genUnsigned Range.linearBounded)
      )
  let
    simOut = C.sampleN (simDuration + 1) (accum @C.System @8 (C.fromList (0 : inp)))
    expected = 0 : init (scanl (+) 0 inp)
  simOut H.=== expected

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
