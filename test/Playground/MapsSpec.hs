module Playground.MapsSpec (spec) where

import Test.Hspec
import Data.Map
import Playground.Maps

spec :: Spec
spec = do

  describe "frequencyMap" $ do
    it "returns a Map with the number of occurrences of an item in a list" $ do
      let input = ["a", "b", "c", "b", "c", "b", "a", "x"]
          expectedResult = fromList [("a", 2), ("b", 3), ("c", 2), ("x", 1)]
      frequencyMap input `shouldBe` expectedResult

  describe "groupByListOfTuple" $ do
    it "groups a list of items by a common key" $ do
      let input = [(1, "a"), (2, "b"), (1, "b"), (3, "a"), (2, "c"), (1, "a")]
          expectedResult = [(1, ["a", "b", "a"]), (2, ["b", "c"]), (3, ["a"])]
      groupByListOfTuple input `shouldBe` expectedResult

  describe "myGroupBy" $ do
    it "groups a list of items into a Map by a common key" $ do
      let input = [(1, "a"), (2, "b"), (1, "b"), (3, "a"), (2, "c"), (1, "a")]
          expectedResult = fromList [(1, ["a", "b", "a"]), (2, ["b", "c"]), (3, ["a"])]
      myGroupBy input `shouldBe` expectedResult
