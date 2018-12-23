module AC.Dec18Spec (spec) where

import Test.Hspec
import AC.Dec18
import Helpers
import Data.List (intercalate)
import qualified Data.Map as M

spec :: Spec
spec = do

  let smallArea = [ "#."
                  , "||"
                  , "#."
                  ]
  let smallMap = M.fromList [ (Coord 0 0, Lumberyard), (Coord 1 0, Open)
                            , (Coord 0 1, Wooded), (Coord 1 1, Wooded)
                            , (Coord 0 2, Lumberyard), (Coord 1 2, Open)
                            ]

  describe "part1" $ do
    it "solves the example" $ do      
      rows <- stringsFromFile "test/data/dec18_input_0.txt"
      part1 rows `shouldBe` 1147

  describe "createCoordMap" $ do
    it "creates a coordinate map" $ do
      createCoordMap smallArea `shouldBe` smallMap

  describe "renderArea" $ do
    it "renders the area to a string" $ do
      renderArea smallMap `shouldBe` intercalate "\n" smallArea


  describe "surroundingCounts" $ do
    let areaMap = createCoordMap [ "#.|"
                                 , "|.."
                                 , ".|."
                                 ]
    it "counts the surrounding types from the center" $ do
      surroundingCounts areaMap (Coord 1 1) `shouldBe` (4, 3, 1)

    it "counts the surrounding types from a corner" $ do
      surroundingCounts areaMap (Coord 0 0) `shouldBe` (2, 1, 0)

  describe "acreNewState" $ do
    describe "open area" $ do
      let originalArea = createCoordMap [ "|||"
                                        , "..."
                                        , "..."
                                        ]
      it "fills an open area with trees when >= 3 adjacent are wooded" $ do
        acreNewState originalArea (Coord 1 1) `shouldBe` Wooded

      it "leaves an open area alone when <= 3 adjacent are wooded" $ do
        acreNewState originalArea (Coord 1 2) `shouldBe` Open

    describe "wooded area" $ do
      let originalArea = createCoordMap [ "###"
                                        , ".|."
                                        , ".|."
                                        ]
      it "fills a wooded area w/ lumberyard when >= 3 adjacent are lumber" $ do
        acreNewState originalArea (Coord 1 1) `shouldBe` Lumberyard

      it "leaves a wooded area alone when <= 3 adjacent are lumber" $ do
        acreNewState originalArea (Coord 1 2) `shouldBe` Wooded

    describe "lumberyard" $ do
      let originalArea = createCoordMap [ "###"
                                        , "|.."
                                        , ".|."
                                        ]
      it "opens a lumberyard if not adjacent to lumberyard and trees" $ do
        acreNewState originalArea (Coord 2 0) `shouldBe` Open

      it "leaves a lumberyard alone when adjacent to lumberyard and trees" $ do
        acreNewState originalArea (Coord 0 0) `shouldBe` Lumberyard

  describe "newState" $ do
    it "determine the example state after one second" $ do
      rows0 <- stringsFromFile "test/data/dec18_input_0.txt"
      rows1 <- stringsFromFile "test/data/dec18_input_1.txt"
      let initialState = createCoordMap rows0
          state1 = createCoordMap rows1
      newState initialState `shouldBe` state1
