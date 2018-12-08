module AC.Dec04Spec (spec) where

import Test.Hspec
import AC.Dec04
import Helpers (thingsFromFile)
import Data.Map (fromList, lookup)
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  let exampleObservations =
        [ Observation { timestamp = "1518-11-01 00:00"
                      , eventType = GuardChange
                      , guardId = 10
                      }
        , Observation { timestamp = "1518-11-01 00:05"
                      , eventType = Sleep
                      , guardId = -1
                      }
        , Observation { timestamp = "1518-11-01 00:25"
                      , eventType = Wake
                      , guardId = -1
                      }
        ]

  describe "part1" $ do
    it "solves the example" $ do
      observations <- thingsFromFile "test/data/dec04_input.txt" parseObservation
      part1 observations `shouldBe` 240

  describe "parseObservation" $ do
    it "converts a line of text into an Observation" $ do
      let inputStrings =
            [ "[1518-11-01 00:00] Guard #10 begins shift"
            , "[1518-11-01 00:05] falls asleep"
            , "[1518-11-01 00:25] wakes up"
            ]
      map parseObservation inputStrings `shouldBe` exampleObservations

  describe "parseMinute" $ do
    it "parses the minute of of a timestamp" $ do
      let timestamp = "1518-11-01 00:05"
      parseMinute timestamp `shouldBe` 5

  describe "processObservations" $ do
    it "returns a guardId and list of slept minutes from a list of observations" $ do
      let expectedResult = [(10, [5..24])]
          initialState = State{currentGuard  = -1, sleptMinute = -1}
      processObservations exampleObservations (State 0 0) [] `shouldBe` expectedResult

  describe "mostMinutesAsleep" $ do
    it "finds the guard who has slept the most" $ do
      let observationMap = fromList [(10, [5..24]), (6, [1..50]), (14, [7..8])]
      mostMinutesAsleep observationMap `shouldBe` 6

  describe "mostFrequentMinuteAsleep" $ do
    it "finds the minute a guard slept most frequently" $ do
      let minutesAsleep = [1, 2, 1, 3, 2, 4, 2, 5, 2, 5]
      mostFrequentMinuteAsleep minutesAsleep `shouldBe` 2

  describe "part2" $ do
    it "solves the example" $ do
      observations <- thingsFromFile "test/data/dec04_input.txt" parseObservation
      part2 observations `shouldBe` 4455

  describe "minuteTotals" $ do
    it "returns a map of minute to total number of minutes slept" $ do
      let observationMap = fromList [(10, [5..24]++[20..30]), (6, [1..50])]
          minuteTotalMap = minuteTotals observationMap
          guardTenMinutes = fromJust (Data.Map.lookup 10 minuteTotalMap)
      -- Since there are 60 entries, only assert a few
      snd(guardTenMinutes !! 3) `shouldBe` 0
      snd(guardTenMinutes !! 5) `shouldBe` 1
      snd(guardTenMinutes !! 23) `shouldBe` 2
      snd(guardTenMinutes !! 31) `shouldBe` 0

  describe "mostFrequentMinuteEntry" $ do
    it "finds the entry with the most frequently slept minute" $ do
      let entry1 = (1, [(1, 0), (23, 2), (31, 0)])
          entry2 = (2, [(1, 1), (2, 2), (31, 0)])
          entry3 = (3, [(1, 2), (8, 7), (31, 2)])
          minuteTotals = fromList [entry1, entry2, entry3]
      mostFrequentMinuteEntry minuteTotals `shouldBe` entry3
