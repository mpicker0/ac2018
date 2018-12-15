module Playground.DataMapSpec (spec) where

import Test.Hspec
import Data.Map
import Data.List
import Playground.Maps

spec :: Spec
spec = do
  -- Examples from http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html#v:insertWith
  -- This is all from Data.Map
  describe "insertWith" $ do
    it "returns a Map where an existing value is updated with the function result" $ do
      let fn = (++)
          key = 5
          value = "xxx"
          originalMap = fromList [(5, "a"),    (3, "b")]
          updatedMap  = fromList [(5, "xxxa"), (3, "b")]
      insertWith fn key value originalMap `shouldBe` updatedMap
      -- compact representation
      insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) `shouldBe` fromList [(3, "b"), (5, "xxxa")]

    it "returns a Map where a nonexisting value is simply added" $ do
      let fn = (++)
          key = 7
          value = "xxx"
          originalMap = fromList [(5, "a"), (3, "b")]
          updatedMap  = fromList [(5, "a"), (3, "b"), (7, "xxx")]
      insertWith fn key value originalMap `shouldBe` updatedMap

    -- This is just a specialized version of the previous test
    it "returns a Map with a single item when run on an empty map" $ do
      insertWith (++) 7 "xxx" empty `shouldBe` singleton 7 "xxx"

  describe "toList" $ do
    -- Not too impressive, since the typical way to input literal maps is via
    -- fromList, the inverse of toList.  Notice that toList exports the keys in
    -- ascending order.
    it "turns a Map into a list of tuples" $ do
      let input = fromList [(5, "a"), (3, "b")]
      toList input `shouldBe` [(3, "b"), (5, "a")]

  describe "fromListWith" $ do
    -- insertWith lets us define a function to handle insert collisions; this
    -- is like fromList but lets us handle collisions
    it "handles collisions between keys" $ do
      let fn = (++)
          input = [(5, "a"), (5, "b"), (3, "b"), (3, "a"), (5, "a")]
          expectedResult = fromList [(3, "ab"), (5, "aba")]
      fromListWith fn input `shouldBe` expectedResult

  describe "map" $ do
    it "maps a function over the values in a map" $ do
      let input = fromList [("a", 1), ("b", 2), ("c", 3)]
          expectedResult = fromList [("a", 2), ("b", 3), ("c", 4)]
      Data.Map.map (+1) input `shouldBe` expectedResult

    it "maps a more complex function over the values in a map" $ do
      let input = fromList [("a", [1, 2, 3]), ("b", [2]), ("c", [3, 4])]
          expectedResult = fromList [("a", [1, 3]), ("b", []), ("c", [3, 4])]
      Data.Map.map (Data.List.delete 2) input `shouldBe` expectedResult
