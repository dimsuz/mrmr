module MrMr.ParseSpec (spec) where

import Data.Text
import Parse
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import TextShow

newtype Header = Header Text
  deriving (Eq, Show)

instance Arbitrary Header where
  arbitrary = do
    (s1, c1) <- arbitrary :: Gen (Int, Int)
    (s2, c2) <- arbitrary :: Gen (Int, Int)
    rest <- arbitrary :: Gen Text
    pure
      ( Header $
          "@@ -"
            <> showt s1
            <> ","
            <> showt c1
            <> " +"
            <> showt s2
            <> ","
            <> showt c2
            <> " @@ "
            <> rest
      )

spec :: Spec
spec = do
  describe "parseDiffHeader" $ do
    prop "parses correctly" $ \(Header text) ->
      encodeHunkHeader (decodeHunkHeader text) `shouldBe` text
