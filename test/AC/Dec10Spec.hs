module AC.Dec10Spec (spec) where

import Test.Hspec
import AC.Dec10
import Helpers

spec :: Spec
spec = do

  -- I didn't write a test for part1; I tested manually ;)

  describe "parsePoint" $ do
    it "parses a Point from a string" $ do
      let input = "position=< 7,  0> velocity=<-1,  0>"
          expectedPoint = Point { position = [7, 0], velocity = [-1, 0] }
      parsePoint input `shouldBe` expectedPoint

  -- describe "newPosition" $ do
  --   it "calculates the new position of a point" $ do
  --     let start = Point { position = [3, 9], velocity = [1, -2] }
  --     newPosition start `shouldBe` [4, 7]

  describe "newPoint" $ do
    it "calculates the new value of a point" $ do
      let start = Point { position = [3, 9], velocity = [1, -2] }
      newPoint start `shouldBe` Point { position = [4, 7], velocity = [1, -2] }

    it "calculates the new point given in the example" $ do
      let start = Point { position = [3, 9], velocity = [1, -2] }
          end = iterate newPoint start !! 3
      end `shouldBe` Point { position = [6, 3], velocity = [1, -2] }

  describe "renderPoints" $ do
    it "renders the points to a string" $ do
      let points = [ Point [-2, 1] [0, 0]
                   , Point [1, 0]  [0, 0]
                   , Point [2, -1] [0, 0]
                   ]
          expectedOutput = "....#\n" ++
                           "...#.\n" ++
                           "#...."
      renderPoints points `shouldBe` expectedOutput

    it "renders the points to the same string even if they are offset" $ do
      let points = [ Point [8, 11] [0, 0]
                   , Point [11, 10]  [0, 0]
                   , Point [12, 9] [0, 0]
                   ]
          expectedOutput = "....#\n" ++
                           "...#.\n" ++
                           "#...."
      renderPoints points `shouldBe` expectedOutput

  -- I was going to write a test for part 2, but looks like I have it already!
  describe "findSmallest" $ do
    it "finds the time of the smallest area given the example data" $ do
      points <- thingsFromFile "test/data/dec10_input.txt" parsePoint
      findSmallest points `shouldBe` 3

  -- TODO this was for testing/exploration only; it isn't a real test
  -- describe "prints a particular iteration" $ do
  --   it "prints the message in the example" $ do
  --     points <- thingsFromFile "test/data/dec10_input.txt" parsePoint
  --     let next ps = map (\p -> newPoint p) ps
  --         end = iterate next points !! 3
  --     renderPoints end `shouldBe` ""
