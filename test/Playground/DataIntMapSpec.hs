module Playground.DataIntMapSpec (spec) where

import Test.Hspec
import Data.IntMap
import Data.List
import Control.Exception (evaluate)

-- The astute reader will note that these tests are exactly the same as for
-- Data.Map.

spec :: Spec
spec = do
  describe "looking up items" $ do
    let testMap = fromList [(1, "one"), (2, "two")]

    it "lets me look up an item using lookup" $ do
      Data.IntMap.lookup 1 testMap `shouldBe` Just("one")
      Data.IntMap.lookup 3 testMap `shouldBe` Nothing

    it "lets me look up an item using !?" $ do
      -- infix syntax for lookup
      testMap !? 1 `shouldBe` Just("one")
      testMap !? 3 `shouldBe` Nothing

    it "lets me look up an item using !" $ do
      -- this returns the actual item, rather than Maybe; it throws an error if
      -- the item is not found
      testMap ! 1 `shouldBe` "one"
      evaluate (testMap ! 3) `shouldThrow` anyException
