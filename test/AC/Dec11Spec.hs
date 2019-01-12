module AC.Dec11Spec (spec) where

import Test.Hspec
import AC.Dec11
import Helpers

spec :: Spec
spec = do

  -- commenting as these tests are slow
  -- describe "part1" $ do
  --   it "solves example 1" $ do
  --     part1 18 `shouldBe` "33,45"
  --
  --   it "solves example 2" $ do
  --     part1 42 `shouldBe` "21,61"

  describe "determinePower" $ do
    it "determines the power for example 1" $ do
      determinePower (Coord 3 5) 8 `shouldBe` 4

    it "determines the power for example 2" $ do
      determinePower (Coord 122 79) 57 `shouldBe` -5

    it "determines the power for example 3" $ do
      determinePower (Coord 217 196) 39 `shouldBe` 0

    it "determines the power for example 4" $ do
      determinePower (Coord 101 153) 71 `shouldBe` 4

  -- Commenting as these tests are slow.  So slow in fact that I've never run
  -- them; I only assume they would work.
  -- describe "maxPower2" $ do
  --   it "determines maxPower for example 1" $ do
  --     maxPower2 18 `shouldBe` (113, (Coord 90 269), 16)
  --
  --   it "determines maxPower for example 2" $ do
  --     maxPower 42 `shouldBe` (30, (Coord 21 61))
