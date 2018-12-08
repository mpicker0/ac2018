module Playground.SortSpec (spec) where

import Test.Hspec
import Data.Ord (comparing)
import Data.List (sortBy)

spec :: Spec
spec = do
  describe "sorting" $ do
    let input = [(1, "a"), (2, "c"), (3, "b")]
    let expectedOutputByFst = [(1, "a"), (2, "c"), (3, "b")]
    let expectedOutputBySnd = [(1, "a"), (3, "b"), (2, "c")]

    it "lets me sort by a particular key" $ do
      sortBy (comparing snd) input `shouldBe` expectedOutputBySnd
