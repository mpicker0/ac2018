module Playground.ListSpec (spec) where

import Test.Hspec
import Data.List (groupBy, sort)

spec :: Spec
spec = do
  describe "adding" $ do
    let input = [1, 2, 3]
    let expectedOutput = [1, 2, 3, 4]

    it "lets me prepend an item to a list" $ do
      1 : [2, 3, 4] `shouldBe` expectedOutput

    it "lets me append an item to a list" $ do
      -- You don't actually append an item; you append a new list that happens
      -- to have one item
      input ++ [4] `shouldBe` expectedOutput

  describe "groupBy" $ do
    let input = ['a', 'a', 'b', 'a', 'b', 'b']

    it "groups adjacent similar elements" $ do
      -- Note, it groups _adjacent_ similar elements
      let expectedOutput = ["aa", "b", "a", "bb"]
      groupBy (==) input `shouldBe` expectedOutput

    it "groups by similar overall elements if I sort first" $ do
      let expectedOutput = ["aaa", "bbb"]
      groupBy (==) (sort input) `shouldBe` expectedOutput
