module AC.Dec05Spec (spec) where

import Test.Hspec
import AC.Dec05

spec :: Spec
spec = do
  let examplePolymer = "dabAcCaCBAcCcaDA"

  describe "part1" $ do
    it "solves the example" $ do
      part1 examplePolymer `shouldBe` 10

  describe "collapse" $ do
    it "removes a single pair at the beginning" $ do
      collapse "aAbc" `shouldBe` "bc"

    it "removes a single pair at the end" $ do
      collapse "bcaA" `shouldBe` "bc"

    it "removes multiple pairs" $ do
      collapse "baAbaAc" `shouldBe` "bbc"

    it "removes multiple adjacent pairs" $ do
      collapse "baAaAc" `shouldBe` "bc"

    it "removes all pairs collapsing from the start" $ do
      collapse "aAbBAa" `shouldBe` ""

    it "removes all pairs collapsing from the middle" $ do
      collapse "aBcCbA" `shouldBe` ""

    it "remove no pairs" $ do
      collapse "abcde" `shouldBe` "abcde"

  describe "part2" $ do
    it "solves the example" $ do      
      part2 examplePolymer `shouldBe` 4

  describe "removeInstances" $ do
    it "removes all instances of a unit from a polymer" $ do
      removeInstances 'a' examplePolymer `shouldBe` "dbcCCBcCcD"
