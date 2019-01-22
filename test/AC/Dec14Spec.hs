module AC.Dec14Spec (spec) where

import Test.Hspec
import AC.Dec14
import Helpers
import Data.IntMap (fromList)

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the example (2018)" $ do
      fileContents <- stringsFromFile "test/data/dec14_input.txt"
      let recipeCount = read (head fileContents) :: Int
      part1 recipeCount `shouldBe` "5941429882"

  describe "getNewState" $ do
    it "moves the state forward by one" $ do
      getNewState initialState `shouldBe` State 0 1 (toIntMap [3, 7, 1, 0]) 4

    it "moves the state forward by two" $ do
      let state2 = iterate getNewState initialState !! 2
      state2 `shouldBe` State 4 3 (toIntMap [3, 7, 1, 0, 1, 0]) 6

    it "moves the state forward by three" $ do
      let state3 = iterate getNewState initialState !! 3
      state3 `shouldBe` State 6 4 (toIntMap [3, 7, 1, 0, 1, 0, 1]) 7

    it "moves the state forward by four" $ do
      let state4 = iterate getNewState initialState !! 4
      state4 `shouldBe` State 0 6 (toIntMap [3, 7, 1, 0, 1, 0, 1, 2]) 8

  describe "tenAfter" $ do
    it "gets the ten recipes after making 5" $ do
      tenAfter 5 initialState `shouldBe` "0124515891"

    it "gets the ten recipes after making 9" $ do
      tenAfter 9 initialState `shouldBe` "5158916779"

    it "gets the ten recipes after making 18" $ do
      tenAfter 18 initialState `shouldBe` "9251071085"

    it "gets the ten recipes after making 2018" $ do
      tenAfter 2018 initialState `shouldBe` "5941429882"

  describe "lastn" $ do
    it "gets the last 5 numbers in the scoreboard" $ do
      let state = State 0 6 (toIntMap [3, 7, 1, 0, 1, 0, 1, 2]) 8
      lastn 5 state `shouldBe` "01012"

  describe "leftOf" $ do
    it "solves the first example" $ do
      leftOf "51589" initialState `shouldBe` 9

    it "solves the second example" $ do
      leftOf "01245" initialState `shouldBe` 5

    it "solves the third example" $ do
      leftOf "92510" initialState `shouldBe` 18

    it "solves the fourth example" $ do
      leftOf "59414" initialState `shouldBe` 2018

    it "works if the last iteration produced one recipe" $ do
      leftOf "1245158916" initialState `shouldBe` 6

    it "works if the last iteration produced two recipes" $ do
      -- the scoreboard will still be 3710101245158916 in this case
      --                     searching for 0124515891
      leftOf "0124515891" initialState `shouldBe` 5
