module AC.Dec12Spec (spec) where

import Test.Hspec
import AC.Dec12
import Helpers
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the example" $ do
      -- I modified the sample input on the webpage; I added all the rules that
      -- led to no plants, so I could count on all the rules being present while
      -- testing.
      fileContents <- stringsFromFile "test/data/dec12_input.txt"
      let initialState = parseInitialState $ head fileContents
          rules = M.fromList (map parseRule (drop 2 fileContents))
      part1 initialState rules `shouldBe` 325

  describe "parseInitialState" $ do
    it "parses the initial state from the file" $ do
      let input = "initial state: #..#.#..##......###...###"
      parseInitialState input `shouldBe` "#..#.#..##......###...###"

  describe "parseRule" $ do
    it "parses a rule read from the file" $ do
      let input = "...## => #"
      parseRule input `shouldBe` ("...##", '#')

  describe "nextGeneration" $ do
    -- Is this the best way to reuse test data from a file?
    let ioRules = do fileContents <- stringsFromFile "test/data/dec12_input.txt"
                     let rs = M.fromList (map parseRule (drop 2 fileContents))
                     return rs
        generation0 = "#..#.#..##......###...###"
        generation1 = "#...#....#.....#..#..#..#"
        generation2 = "##..##...##....#..#..#..##"
        generation3 = "#.#...#..#.#....#..#..#...#"

    it "calculates generation 1 from the example" $ do
      rules <- ioRules
      nextGeneration rules 0 generation0 `shouldBe` (0, generation1)

    it "calculates generation 2 from the example" $ do
      -- generation 2 has a plant in position 25, past the end of generation 1
      rules <- ioRules
      nextGeneration rules 0 generation1 `shouldBe` (0, generation2)

    it "calculates generation 3 from the example" $ do
      -- generation 3 has a plant in position -1
      rules <- ioRules
      nextGeneration rules 0 generation2 `shouldBe` (-1, generation3)
