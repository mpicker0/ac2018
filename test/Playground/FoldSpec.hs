module Playground.FoldSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do

  describe "foldl" $ do
    it "lets me fold a list into a string" $ do
      let input = [1, 2, 3, 4]
      let expectedOutput = "1234"
      -- Syntax:  foldl <fn (acc cur)> <initial> <list>
      -- where <initial> is the initial value of the accumulator and <list>
      -- is the list to fold over
      foldl (\acc x -> acc ++ show x) "" input `shouldBe` expectedOutput

  describe "fold ignoring value of list" $ do
    it "folds a list where I don't care about the iterator value" $ do
      foldl (\acc _ -> acc ++ "*") "" [1..4] `shouldBe` "****"

    it "does the same thing with iterate" $ do
      -- iterate generates an infinite list; we use !! to take the 4th item
      iterate (++"*") "" !! 4 `shouldBe` "****"
