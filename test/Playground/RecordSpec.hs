module Playground.RecordSpec (spec) where

import Test.Hspec

data Coord = Coord { x :: Int
                   , y :: Int
                   } deriving (Eq, Show)

spec :: Spec
spec = do

  describe "record syntax" $ do
    it "lets me specify record fields positionally or by name" $ do
      Coord 2 3 `shouldBe` Coord { x = 2, y = 3 }

    it "lets me extract fields positionally" $ do
      let coord = Coord 2 3
          (Coord x y) = coord
          -- we can use any name as the target
          (Coord a b) = coord
      (x, y) `shouldBe` (2, 3)
      (a, b) `shouldBe` (2, 3)

    it "lets me extract fields by name" $ do
      let coord = Coord 2 3
      x coord `shouldBe` 2
      y coord `shouldBe` 3

  describe "pattern matching" $ do
    it "lets me pattern match in a function binding" $ do
      let fn :: Coord -> String
          fn (Coord 0 0) = "origin"
          fn (Coord 0 _) = "on x-axis"
          fn (Coord _ 0) = "on y-axis"
          fn (Coord x y) = "(" ++ show(x) ++ ", " ++ show(y) ++ ")"
      fn (Coord 0 0) `shouldBe` "origin"
      fn (Coord 0 1) `shouldBe` "on x-axis"
      fn (Coord 6 0) `shouldBe` "on y-axis"
      fn (Coord 3 4) `shouldBe` "(3, 4)"
