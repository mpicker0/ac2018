module Playground.LinkedListsSpec (spec) where

import Test.Hspec
import Playground.LinkedLists
import Control.Arrow

spec :: Spec
spec = do
  describe "twoItemDList" $ do
    let dlist = twoItemDList 1 2
    -- This builds:  Empty <--> 1 <--> 2 <--> Empty
    it "lets me move forward in a 2-item list" $ do
      value dlist `shouldBe` 1
      value (next dlist) `shouldBe` 2

    it "lets me move backward in a 2-item list" $ do
      let second = next dlist
      value second `shouldBe` 2
      let first = prev second
      value first `shouldBe` 1

  describe "twoItemCList" $ do
    let clist = twoItemCList 1 2
    -- This builds  .-- 1 <--> 2 --.
    --              \--------------/
    it "lets me move forward once" $ do
      cvalue clist `shouldBe` 1
      cvalue (cnext clist) `shouldBe` 2

    it "lets me move backward once" $ do
      cvalue (cprev clist) `shouldBe` 2
      -- also
      let second = cnext clist
      cvalue second `shouldBe` 2
      let first = cprev second
      cvalue first `shouldBe` 1

    it "lets me move forward an arbitrary number of times" $ do
      let result = cnext >>> cnext >>> cnext >>> cvalue $ clist
      result `shouldBe` 2

    it "lets me move backward an arbitrary number of times" $ do
      let result = cprev >>> cprev >>> cprev >>> cvalue $ clist
      result `shouldBe` 2

  describe "oneItemCList" $ do
    let clist = oneItemCList 1
    it "lets me move forward an arbitrary number of times" $ do
      let result = cnext >>> cnext >>> cnext >>> cvalue $ clist
      result `shouldBe` 1

    it "lets me move backward an arbitrary number of times" $ do
      let result = cprev >>> cprev >>> cprev >>> cvalue $ clist
      result `shouldBe` 1
