module AC.Dec03Spec (spec) where

import Test.Hspec
import AC.Dec03
import Data.Map (fromList)

spec :: Spec
spec = do
  let exampleClaims =
        [ Claim {claimId = 1, coord = Coord {x = 1, y = 3}, width = 4, height = 4}
        , Claim {claimId = 2, coord = Coord {x = 3, y = 1}, width = 4, height = 4}
        , Claim {claimId = 3, coord = Coord {x = 5, y = 5}, width = 2, height = 2}
        ]

  describe "part1" $ do
    it "solves the example" $ do
      part1 exampleClaims `shouldBe` 4

  describe "parseClaim" $ do
    it "converts a line of text into a Claim" $ do
      let expectedClaim = Claim {claimId = 1, coord = Coord 1 3, width = 4, height = 4}
      parseClaim "#1 @ 1,3: 4x4" `shouldBe` expectedClaim

  describe "coveredCoords" $ do
    it "finds the coordinates covered by a Claim" $ do
      let claim = Claim {claimId = 3, coord = Coord 5 5, width = 2, height = 2}
          expectedResult = [ Coord 5 5, Coord 5 6, Coord 6 5, Coord 6 6 ]
      coveredCoords claim `shouldBe` expectedResult

  describe "part2" $ do
    it "solves the example" $ do
      part2 exampleClaims `shouldBe` 3

  describe "hasOneClaim" $ do
    let coordToClaimIdsMap = fromList [
            (Coord {x = 1, y = 3},[1]),
            (Coord {x = 3, y = 3},[2,1])
          ]
    it "returns True if a coordinate has exactly one claim" $ do
      hasOneClaim coordToClaimIdsMap Coord {x = 1, y = 3} `shouldBe` True

    it "returns False if a coordinate has more than one claim" $ do
      hasOneClaim coordToClaimIdsMap Coord {x = 3, y = 3} `shouldBe` False

    it "returns False if a coordinate does not exist" $ do
      -- This would never happen but why not test it anyway as I'm learning
      -- Haskell
      hasOneClaim coordToClaimIdsMap Coord {x = 99, y = 99} `shouldBe` False
