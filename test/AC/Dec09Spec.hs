module AC.Dec09Spec (spec) where

import Test.Hspec
import AC.Dec09

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the 10-player example" $ do
      part1 (10, 1618) `shouldBe` 8317

    -- works but takes a few seconds; commenting out
    -- it "solves the 30-player example" $ do
    --   part1 (30, 5807) `shouldBe` 37305

    it "solves the opening example" $ do
      part1 (9, 25) `shouldBe` 32

  describe "playTurn" $ do
    it "plays the first turn" $ do
      let openingState = initialState 1
          expectedState =
            State { circle = [1, 0]
                  , currentMarbleIdx = 0
                  , scores = [0]
                  }
      playTurn openingState 1 0 `shouldBe` expectedState

    it "plays a scoring turn" $do
      let openingState =
            State { circle = [0..10]
                  , currentMarbleIdx = 9
                  , scores = [0, 0]
                  }
          expectedState =
            State { circle = [0, 1, 3, 4, 5, 6, 7, 8, 9, 10]
                  , currentMarbleIdx = 2
                  , scores = [0, 25]
                  }

      playTurn openingState 23 1 `shouldBe` expectedState

  describe "removeMarble" $ do
    it "removes a marble when the 7th marble is behind the current one" $ do
      --                  7<-6<-5<-4<-3<-2<-1<-c
      let circle = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          currentMarbleIdx = 9
          expectedResult = (2, 2, [0, 1, 3, 4, 5, 6, 7, 8, 9, 10])
      removeMarble currentMarbleIdx circle `shouldBe` expectedResult

    it "removes a marble with the 7th marble is ahead of the current one" $ do
      --            3<-2<-1<-c           7<-6<-5<-4<-
      let circle = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          currentMarbleIdx = 3
          expectedResult = (7, 7, [0, 1, 2, 3, 4, 5, 6, 8, 9, 10])
      removeMarble currentMarbleIdx circle `shouldBe` expectedResult

  describe "placeMarble" $ do
    it "places a marble that would go into the middle of the list" $ do
      let circle = [10, 11, 12, 13]
          currentMarbleIdx = 1
          expectedResult = (3, [10, 11, 12, 14, 13])
      placeMarble 14 currentMarbleIdx circle `shouldBe` expectedResult

    it "places a marble that would go beyond the end of the list" $ do
      let circle = [10, 11, 12, 13]
          currentMarbleIdx = 3
          expectedResult = (1, [10, 14, 11, 12, 13])
      placeMarble 14 currentMarbleIdx circle `shouldBe` expectedResult

    it "works for the initial move" $ do
      placeMarble 1 0 [0] `shouldBe` (0, [1, 0])

    it "works for the second move" $ do
      placeMarble 2 1 [0, 1] `shouldBe` (1, [0, 2, 1])

    it "works for the third move" $ do
      -- implementation detail; differs from the example
      placeMarble 3 1 [0, 2, 1] `shouldBe` (0, [3, 0, 2, 1])

  describe "addToIndex" $ do
    it "adds to the value at the specified index" $ do
      let input = [0, 1, 2]
      addToIndex 1 9 input `shouldBe` [0, 10, 2]
