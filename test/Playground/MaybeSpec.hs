module Playground.MaybeSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "fmap (map applied to a functor)" $ do
    it "lets me fmap a function to a value" $ do
      let maybeInt = Just 3 :: Maybe Int
      fmap (+1) maybeInt `shouldBe` Just 4

    it "lets me fmap a function to Nothing" $ do
      let maybeInt = Nothing :: Maybe Int
      fmap (+1) maybeInt `shouldBe` Nothing

  describe "maybe" $ do
    it "keeps the existing value" $ do
      -- the maybe function takes three arguments:  a default value, a function,
      -- and a Maybe instance.  In this case we want to return the value itself
      -- if it exists, so we pass in the id (identity) function.
      let maybeInt = Just 3 :: Maybe Int
      maybe 99 id maybeInt `shouldBe` 3

    it "lets me extract a default value if there is Nothing" $ do
      let maybeInt = Nothing :: Maybe Int
      maybe 99 id maybeInt `shouldBe`  99

  describe "fromJust" $ do
    it "extracts a value if it exists" $ do
      let maybeInt = Just 3 :: Maybe Int
      fromJust maybeInt `shouldBe` 3

    it "throws an exception if it does not exist" $ do
      let maybeInt = Nothing :: Maybe Int
      evaluate (fromJust maybeInt) `shouldThrow` anyException
