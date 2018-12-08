module Playground.MapSpec (spec) where

import Test.Hspec
import Playground.Maps

spec :: Spec
spec = do
  describe "map" $ do
    let input = [1, 2, 3]
    let expectedOutput = [2, 3, 4]

    it "lets me map a function over a list" $ do
      let fn = (+1)
      map fn input `shouldBe` expectedOutput

    it "lets me map a lambda over a list" $ do
      map (\x -> x + 1) input `shouldBe` expectedOutput
      -- this would be the preferred way, over a lambda
      map (+1) input `shouldBe` expectedOutput

  describe "concactMap" $ do
    let listOfListsInput = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]
    let expectedListOfLists = [[2, 3, 4], [2, 3, 4], [2, 3, 4]]
    let concatMappedOutput = [2, 3, 4, 2, 3, 4, 2, 3, 4]

    it "lets me map a list of lists" $ do
      map (map (+1)) listOfListsInput `shouldBe` expectedListOfLists

    -- this is like Scala's flatMap
    it "lets me concatMap a list of lists into a list" $ do
      concatMap (map (+1)) listOfListsInput `shouldBe` concatMappedOutput
