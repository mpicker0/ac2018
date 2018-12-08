module Playground.ListSpec (spec) where

import Test.Hspec
import Data.List (groupBy, maximumBy, sort)
import Data.Ord (comparing)

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

  describe "maximumBy" $ do
    -- also see sortBy in SortSpec
    it "lets me find the maximum in a list by specifying a function" $ do
      let input = [(1, "a"), (2, "c"), (3, "b")]
      maximumBy (comparing fst) input `shouldBe` (3, "b")
      maximumBy (comparing snd) input `shouldBe` (2, "c")
      -- written out
      let fstCompareFn a b = compare (fst a) (fst b)
          sndCompareFn a b = compare (snd a) (snd b)
      maximumBy fstCompareFn input `shouldBe` (3, "b")
      maximumBy sndCompareFn input `shouldBe` (2, "c")

    it "lets me find the maximum in a nested data structure" $ do
      -- this seems rather nasty :(
      let entry1 = (1, [(1, "a"), (2, "c"), (3, "y")])
          entry2 = (2, [(1, "a"), (7, "b"), (4, "d")])
          input = [entry1, entry2]
          nestedCompareFn (_, a) (_, b) =
            compare (snd (maximumBy (comparing snd) a)) (snd (maximumBy (comparing snd) b))
      maximumBy nestedCompareFn input `shouldBe` entry1

    it "lets me find the maximum in a nested data structure - alternate" $ do
      -- not much better
      let entry1 = (1, [(1, "a"), (2, "c"), (3, "y")])
          entry2 = (2, [(1, "a"), (7, "b"), (4, "d")])
          input = [entry1, entry2]
          greatest arr = snd (maximumBy (comparing snd) arr)
          nestedCompareFn (_, a) (_, b) = compare (greatest a) (greatest b)
      maximumBy nestedCompareFn input `shouldBe` entry1
