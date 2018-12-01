module HelpersSpec (spec) where

import Test.Hspec
import Helpers

spec :: Spec
spec = do
  describe "intsFromFile" $ do
    it "reads a list of Ints from a file" $ do
      -- intsFromFile "test/data/numbers.txt" `shouldBe` return([1, 2, 3])
      pendingWith "figure out how to test IO monad stuff"
