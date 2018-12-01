module AC.Dec01Spec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import AC.Dec01

spec :: Spec
spec = do
  describe "part1" $ do
    it "sums up the numbers in the description" $ do
      part1 [1, -2, 3, 1] `shouldBe` 3

  describe "part2" $ do
    it "finds the repeated frequency in the description" $ do
      part2 [1, -2, 3, 1] `shouldBe` 2

    it "finds the result right away" $ do
      part2 [1, -1] `shouldBe` 0

    it "finds the result for example 2" $ do
      part2 [3, 3, 4, -2, -4] `shouldBe` 10

    it "finds the result for example 3" $ do
      part2 [-6, 3, 8, 5, -6] `shouldBe` 5

    it "finds the result for example 4" $ do
      part2 [7, 7, -2, -7, -4] `shouldBe` 14
