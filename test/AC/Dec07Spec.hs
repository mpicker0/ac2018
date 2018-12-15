module AC.Dec07Spec (spec) where

import Test.Hspec
import AC.Dec07
import Helpers
import qualified Data.Map as M

spec :: Spec
spec = do
  let initialMap = M.fromList [
          ("A", ["C"])
        , ("B", ["A"])
        , ("D", ["A"])
        , ("E", ["F", "D", "B"])  -- order here is an implementation detail
        , ("F" ,["C"])
        ]
      initialState =
        State { prereqMap = initialMap
              , candidates = ["C"]
              , finished = []
              }

  describe "part1" $ do
    it "solves the example" $ do
      stepReqs <- thingsFromFile "test/data/dec07_input.txt" parseStepReq
      part1 stepReqs `shouldBe` "CABDFE"

  describe "parseStepReq" $ do
    it "parses a StepReq from a string" $ do
      let input = "Step C must be finished before step A can begin."
      parseStepReq input `shouldBe` StepReq "C" "A"

  describe "buildInitialState" $ do
    it "builds the initial state" $ do
      stepReqs <- thingsFromFile "test/data/dec07_input.txt" parseStepReq
      buildInitialState stepReqs `shouldBe` initialState

  describe "completeStep" $ do
    it "completes the first step (C)" $ do
      let updatedMap = M.fromList [
              ("B", ["A"])
            , ("D", ["A"])
            , ("E", ["F", "D", "B"])  -- order here is an implementation detail
            ]
          expectedState =
            State { prereqMap = updatedMap
                  , candidates = ["A", "F"]  -- Should be sorted
                  , finished = ["C"]
                  }
      completeStep initialState `shouldBe` expectedState

    it "completes step B" $ do
      let initialState =
            State { prereqMap = M.fromList [("E", ["F", "D", "B"])]
                  , candidates = ["B", "D", "F"]
                  , finished = ["C", "A"]
                  }
          expectedState =
            State { prereqMap = M.fromList [("E", ["F", "D"])]
                  , candidates = ["D", "F"]
                  , finished = ["C", "A", "B"]
                  }
      completeStep initialState `shouldBe` expectedState
