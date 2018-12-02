module AC.Dec02Spec (spec) where

import Test.Hspec
import AC.Dec02

spec :: Spec
spec = do
  describe "part1" $ do
    let exampleIds =
          [ "abcdef"
          , "bababc"
          , "abbcde"
          , "abcccd"
          , "aabcdd"
          , "abcdee"
          , "ababab"
          ]

    it "calculates the checksum in the example" $ do
      part1 exampleIds `shouldBe` 12

    it "determines that a boxId contains a letter occuring twice" $ do
      analyzeId "aba" `shouldBe` (True, False)

    it "determines that a boxId contains a letter occuring three times" $ do
      analyzeId "abaca" `shouldBe` (False, True)

    it "determines that a boxId contains a letter occuring two times and three times" $ do
      analyzeId "ababa" `shouldBe` (True, True)

    it "determines that a boxId contains no letters occurring two or three times" $ do
      analyzeId "abcd" `shouldBe` (False, False)

  describe "part2" $ do
    let exampleParts =
          [ "abcde"
          , "fghij"
          , "klmno"
          , "pqrst"
          , "fguij"
          , "axcye"
          , "wvxyz"
          ]

    it "finds the solution in the example" $ do
      part2 exampleParts `shouldBe` "fgij"

    describe "dropNth" $ do
      it "drops the item at index 2" $ do
        dropNth "abcd" 2 `shouldBe` "abd"

    describe "variations" $ do
      it "returns variations of a string with one character removed" $ do
        variations "abcd" `shouldBe` ["bcd", "acd", "abd", "abc"]

    describe "similar" $ do
      it "returns the substring of matching characters for similar strings" $ do
        similar "fghij" "fguij" `shouldBe` ["fgij"]

      it "returns nothing for dissimilar strings" $ do
        similar "abcde" "axcye" `shouldBe` []

      it "returns nothing for identical strings" $ do
        similar "abcde" "abcde" `shouldBe` []
