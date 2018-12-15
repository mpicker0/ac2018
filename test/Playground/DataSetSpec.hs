module Playground.DataSetSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "fromList" $ do
    it "combines multiple identical items" $ do
      Set.fromList [1, 2, 1, 2, 3] `shouldBe` Set.fromList [1, 2, 3]

  describe "difference" $ do
    it "returns the difference between two sets" $ do
      let bigSet = Set.fromList [1, 2, 3, 4]
          smallSet = Set.fromList [1, 2, 3]
      Set.difference bigSet smallSet `shouldBe` Set.fromList [4]
