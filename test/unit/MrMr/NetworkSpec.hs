module MrMr.NetworkSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "parseDiffHeader" $ do
    it "parses line/count on both sides" $ do
      "foo bar" `shouldBe` "foo bar"
