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
      iterate (\x -> "*" ++ x) "" !! 4 `shouldBe` "****"
      -- or more concisely
      iterate (++"*") "" !! 4 `shouldBe` "****"

  describe "folding pairs of items" $ do
    it "folds pairs of items into their sum" $ do
      let input = [1, 2, 3, 4, 5, 6]
          result = foldl (\(out, (a:b:t)) _ ->
                     (out ++ [a + b], t)) ([], input) [1..length input `div` 2]
      result `shouldBe` ([3, 7, 11], [])

    it "does the same thing with iterate" $ do
      let input = [1, 2, 3, 4, 5, 6]
          outlen = length input `div` 2
          initial = ([], input)
          result =
            iterate (\(out, (a:b:t)) -> (out ++ [a + b], t)) initial !! outlen
      result `shouldBe` ([3, 7, 11], [])

  describe "scanl" $ do
    it "lets me turn a list of numbers into successive lists of strings" $ do
      -- see the first example under foldl above
      let input = [1, 2, 3, 4]
      -- the difference between foldl and scanl, is that scanl returns all the
      -- intermediate results
      let expectedOutput = ["", "1", "12", "123", "1234"]
      scanl (\acc x -> acc ++ show x) "" input `shouldBe` expectedOutput

  describe "until" $ do
    it "applies a function until a predicate is met" $ do
      -- similar to iterate, but there is a predicate that tells when to stop
      -- Syntax:  until <stop predicate> <fn> <initial>
      -- See iterate above
      until (\x -> length x == 4) (\x -> "*" ++ x) "" `shouldBe` "****"
      -- or more concisely
      until (\x -> length x == 4) (++"*") "" `shouldBe` "****"
