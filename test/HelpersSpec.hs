module HelpersSpec (spec) where

import Test.Hspec
import Helpers
import Data.Map (fromList)

spec :: Spec
spec = do
  describe "intsFromFile" $ do
    it "reads a list of Ints from a file" $ do
      ints <- intsFromFile "test/data/numbers.txt"
      ints `shouldBe` [1, 2, 3, 4]

  describe "occurrences" $ do
    it "counts the number of items in a list that satisfy a predicate" $ do
      occurrences(=='1') "10101" `shouldBe` 3

  describe "frequencyMap" $ do
    it "returns a Map with the number of occurrences of an item in a list" $ do
      let input = ["a", "b", "c", "b", "c", "b", "a", "x"]
          expectedResult = fromList [("a", 2), ("b", 3), ("c", 2), ("x", 1)]
      frequencyMap input `shouldBe` expectedResult
