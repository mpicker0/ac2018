module AC.Dec13Spec (spec) where

import Test.Hspec
import AC.Dec13
import Helpers
import qualified Data.Map as M
import Data.List (sort)

spec :: Spec
spec = do
  describe "part1" $ do
    it "solves the example" $ do
      rows <- stringsFromFile "test/data/dec13_input.txt"
      let initialState = getInitialState(createTrackMap rows)
      part1 initialState `shouldBe` "7,3"

  let straightMap = M.fromList [
                      (Coord 0 0, Vert)
                    , (Coord 0 1, CartS)
                    , (Coord 0 2, Vert)
                    , (Coord 0 3, Vert)
                    , (Coord 0 4, Vert)
                    , (Coord 0 5, CartN)
                    , (Coord 0 6, Vert)
                    ]
  let straightMapState = State { trackMap = M.fromList [
                                    (Coord 0 0, Vert)
                                  , (Coord 0 1, Vert)
                                  , (Coord 0 2, Vert)
                                  , (Coord 0 3, Vert)
                                  , (Coord 0 4, Vert)
                                  , (Coord 0 5, Vert)
                                  , (Coord 0 6, Vert)
                                  ]
                               , carts = [
                                   Cart (Coord 0 1) S TurnLeft
                                 , Cart (Coord 0 5) N TurnLeft
                                 ]
                               }

  describe "createTrackMap" $ do
    it "reads the input file and creates a map" $ do
      rows <- stringsFromFile "test/data/dec13_straight_track.txt"
      createTrackMap rows `shouldBe` straightMap

  describe "getInitialState" $ do
    it "separates the carts from the raw track map" $ do
      getInitialState straightMap `shouldBe` straightMapState

  describe "moveCart" $ do
    describe "straight line" $ do
      it "continues north" $ do
        let initialCart = Cart (Coord 0 1) N GoStraight
            trackMap = M.fromList [(Coord 0 1, Vert)]
            expectedCart = Cart (Coord 0 0) N GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "continues south" $ do
        let initialCart = Cart (Coord 0 0) S GoStraight
            trackMap = M.fromList [(Coord 0 0, Vert)]
            expectedCart = Cart (Coord 0 1) S GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "continues east" $ do
        let initialCart = Cart (Coord 0 0) E GoStraight
            trackMap = M.fromList [(Coord 0 0, Horiz)]
            expectedCart = Cart (Coord 1 0) E GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "continues west" $ do
        let initialCart = Cart (Coord 1 0) W GoStraight
            trackMap = M.fromList [(Coord 1 0, Horiz)]
            expectedCart = Cart (Coord 0 0) W GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

    describe "curve / (swne)" $ do
      let start = Coord 1 1
      let trackMap = M.fromList [(start, SWNE)]

      it "curves northward" $ do
        let initialCart = Cart start E GoStraight
            expectedCart = Cart (Coord 1 0) N GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "curves southward" $ do
        let initialCart = Cart start W GoStraight
            expectedCart = Cart (Coord 1 2) S GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "curves eastward" $ do
        let initialCart = Cart start N GoStraight
            expectedCart = Cart (Coord 2 1) E GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "curves westward" $ do
        let initialCart = Cart start S GoStraight
            expectedCart = Cart (Coord 0 1) W GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

    describe "curve \\ (nwse)" $ do
      let start = Coord 1 1
      let trackMap = M.fromList [(start, NWSE)]

      it "curves northward" $ do
        let initialCart = Cart start W GoStraight
            expectedCart = Cart (Coord 1 0) N GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "curves southward" $ do
        let initialCart = Cart start E GoStraight
            expectedCart = Cart (Coord 1 2) S GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "curves eastward" $ do
        let initialCart = Cart start S GoStraight
            expectedCart = Cart (Coord 2 1) E GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

      it "curves westward" $ do
        let initialCart = Cart start N GoStraight
            expectedCart = Cart (Coord 0 1) W GoStraight
        moveCart trackMap initialCart `shouldBe` expectedCart

  describe "intersection" $ do
    -- Cart is traveling east and has arrived at an intersection
    let start = Coord 1 1
    let trackMap = M.fromList [(start, Cross)]

    it "turns left (north) the first time" $ do
      let initialCart = Cart start E TurnLeft
          expectedCart = Cart (Coord 1 0) N GoStraight
      moveCart trackMap initialCart `shouldBe` expectedCart

    it "goes straight the second time" $ do
      let initialCart = Cart start E GoStraight
          expectedCart = Cart (Coord 2 1) E TurnRight
      moveCart trackMap initialCart `shouldBe` expectedCart

    it "goes right the third time" $ do
      let initialCart = Cart start E TurnRight
          expectedCart = Cart (Coord 1 2) S TurnLeft
      moveCart trackMap initialCart `shouldBe` expectedCart

  describe "moveCarts" $ do
    it "moves the carts forward one tick" $ do
      let expectedState =
            straightMapState { carts = [ Cart (Coord 0 2 ) S TurnLeft
                                       , Cart (Coord 0 4 ) N TurnLeft ]}
      moveCarts straightMapState `shouldBe` expectedState

    it "doesn't move a collided cart" $ do
      let initialState =
            straightMapState { carts = [ Cart (Coord 0 0) S GoStraight
                                       , Cart (Coord 0 1) N GoStraight ]}
          expectedState =
            initialState { carts = [ Cart (Coord 0 1) S GoStraight
                                   , Cart (Coord 0 1) N GoStraight]}
      carts (moveCarts initialState) `shouldBe` carts expectedState

    it "moves the carts in the correct order" $ do
      let initialState =
            straightMapState { carts = [ Cart (Coord 0 1) N GoStraight
                                       , Cart (Coord 0 0) S GoStraight ]}
          expectedState =
            initialState { carts = [ Cart (Coord 0 1) S GoStraight
                                   , Cart (Coord 0 1) N GoStraight ]}
      carts (moveCarts initialState) `shouldBe` carts expectedState

  describe "moveUntilCollision" $ do
    it "detects a collision on a straight track" $ do
      let expectedCarts = [ Cart (Coord 0 3) S TurnLeft
                          , Cart (Coord 0 3) N TurnLeft ]
      carts (moveUntilCollision straightMapState) `shouldBe` expectedCarts

  describe "ordering of Coord" $ do
    it "sorts Coord by y, then by x" $ do
      let original = [ (Coord 1 1), (Coord 1 0), (Coord 0 0), (Coord 0 1) ]
          sorted = [ (Coord 0 0), (Coord 1 0), (Coord 0 1), (Coord 1 1) ]
      sort original `shouldBe` sorted

  describe "part2" $ do
    it "solves the example" $ do
      rows <- stringsFromFile "test/data/dec13_part_2.txt"
      let initialState = getInitialState(createTrackMap rows)
      part2 initialState `shouldBe` "6,4"

  describe "moveThenRemoveCollision" $ do
    it "moves until a collision is encountered, then removes it" $ do
      rows <- stringsFromFile "test/data/dec13_part_2.txt"
      let initialState = getInitialState(createTrackMap rows)
          expectedCarts = [ Cart (Coord 2 2) W TurnLeft
                          , Cart (Coord 2 6) W TurnLeft
                          , Cart (Coord 6 6) E TurnLeft
                          ]
          (State _ carts) = moveThenRemoveCollision initialState
      carts `shouldBe` expectedCarts

    it "agrees with the second example after two runs" $ do
      rows <- stringsFromFile "test/data/dec13_part_2.txt"
      let initialState = getInitialState(createTrackMap rows)
          expectedCarts = [ Cart (Coord 6 4) N TurnLeft ]
          (State _ carts) = iterate moveThenRemoveCollision initialState !! 2
      carts `shouldBe` expectedCarts
