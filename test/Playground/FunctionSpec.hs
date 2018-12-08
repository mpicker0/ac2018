module Playground.FunctionSpec (spec) where

import Test.Hspec
import Control.Arrow

spec :: Spec
spec = do
  describe "functions" $ do
    let increment x = x + 1
    let double x = x * 2
    let asString x = show x
    let decorate s = "->" ++ s ++ "<-"

    it "lets me call a function normally" $ do
      increment 3 `shouldBe` 4
      double 3 `shouldBe` 6

    it "lets me compose functions using dot" $ do
      -- popular in other languages
      increment (double 3) `shouldBe` 7
      -- using compose operator
      (increment . double) 3 `shouldBe` 7

    it "lets me use a nicer syntax with dollar" $ do
      -- things to the left of the $ are evaluated first, eliminating the need
      -- for the parentheses
      let result = increment . double $ 3
      result `shouldBe` 7

    it "lets me chain together many functions" $ do
      -- What might be confusing about this is we have to read the functions
      -- from right to left; "increment . double $ 3" means to start with 3,
      -- then double it, then increment it.  If we want to do several operations
      -- it looks like this:
      let result = decorate . asString . increment . double $ 3
      result `shouldBe` "->7<-"

    it "lets me chain together many functions using arrows" $ do
      -- I think this is more readable.  The >>> operator comes from the
      -- "import Control.Arrow" at the top of the file.
      let result = double >>> increment >>> asString >>> decorate $ 3
      result `shouldBe` "->7<-"
