module SampleSpec where

import Test.Hspec.Core.Spec (context, describe, it, Spec)
import Test.Hspec.Expectations (shouldBe)


spec :: Spec
spec = describe "check the function: plus" $ do
  context "when 1+1 was given" $ do
    it "should return 2" $ do
      2 `shouldBe` 2
    it "should return 2" $ do
      2 `shouldBe` 0
