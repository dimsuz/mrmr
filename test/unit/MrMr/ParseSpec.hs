module MrMr.ParseSpec (spec) where

import Data.Text
import Numeric.Natural
import Parse
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural
import Test.QuickCheck.Instances.Text
import TextShow

newtype Header = Header Text
  deriving (Eq, Show)

instance Arbitrary Header where
  arbitrary = do
    (Positive s1, Positive c1) <- arbitrary :: Gen (Positive Int, Positive Int)
    (Positive s2, Positive c2) <- arbitrary :: Gen (Positive Int, Positive Int)
    rest <- arbitrary :: Gen Text
    let old = if c1 == 1 then showt s1 else showt s1 <> "," <> showt c1
        new = if c2 == 1 then showt s2 else showt s2 <> "," <> showt c2
    pure
      ( Header $
          "@@ -"
            <> old
            <> " +"
            <> new
            <> " @@ "
            <> rest
      )

spec :: Spec
spec = do
  describe "parseDiffHeader" $ do
    prop "parses correctly" $ \(Header text) ->
      encodeHunkHeader <$> decodeHunkHeader text `shouldBe` Right text
