module Playground.FoldSpec (spec) where

import Test.Hspec
import Playground.Maps

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
