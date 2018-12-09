module Playground.TextSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "replace" $ do
    -- Things in Data.Text operate on Text, not String.  We can convert a String
    -- to a Text using pack.
    let input = T.pack "abba"

    it "replaces a substring" $ do
      let target = T.pack "a"
          replacement = T.pack "x"
      T.replace target replacement input `shouldBe` T.pack "xbbx"
