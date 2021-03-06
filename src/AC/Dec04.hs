module AC.Dec04
  ( part1
  , part2
  , EventType (..)
  , Observation (..)
  , State (..)
  , parseObservation
  , parseMinute
  , processObservations
  , mostMinutesAsleep
  , mostFrequentMinuteAsleep
  , minuteTotals
  , mostFrequentMinuteEntry
  ) where

import System.Environment
import Helpers
import Control.Arrow
import Data.List
import Data.Map (Map, fromListWith, toList, lookup)
import qualified Data.Map as M (map)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Regex.Posix

main = do
    [problem, filename] <- getArgs
    observations <- thingsFromFile filename parseObservation
    putStrLn (case problem of
               "1" -> show (part1 observations)
               "2" -> show (part2 observations))

data EventType = GuardChange | Sleep | Wake deriving (Eq, Ord, Show)
data Observation = Observation { timestamp :: String
                               , eventType :: EventType
                               , guardId   :: Int
                               } deriving (Eq, Ord, Show)
data State = State { currentGuard :: Int
                   , sleptMinute :: Int
                   } deriving (Show)

part1 :: [Observation] -> Int
part1 observations =
  let observationMap = guardIdToMinuteArray (sort observations)
      heaviestSleeper = mostMinutesAsleep observationMap
      mostFrequentMinute =
        fmap mostFrequentMinuteAsleep (Data.Map.lookup heaviestSleeper observationMap)
  in (fromJust mostFrequentMinute) * heaviestSleeper

-- Return the ID of the guard who was asleep the most
mostMinutesAsleep :: Map Int [Int] -> Int
mostMinutesAsleep observationMap =
  let compareEntry (_, arr1) (_, arr2) = compare (length arr1) (length arr2)
      (heavySleeper, _) = maximumBy compareEntry (toList observationMap)
  in heavySleeper

mostFrequentMinuteAsleep :: [Int] -> Int
mostFrequentMinuteAsleep minutesAsleep =
  let asleepList = toList (frequencyMap minutesAsleep)
      mostFrequent = maximumBy (comparing snd) >>> fst $ asleepList
  in mostFrequent

-- From a list of Observation, return a map of guardId to minutes slept
guardIdToMinuteArray :: [Observation] -> Map Int [Int]
guardIdToMinuteArray observations =
  let allObservations = processObservations observations (State 0 0) []
  in fromListWith (++) (sort allObservations)

-- Internally used to process the list of observations.  We look at each item
-- and update the state, and when applicable, add to the guardId-to-minutes
-- tuple.  Parameters:  remaining observations, state, processed
-- guardId-to-minutes
processObservations :: [Observation] -> State -> [(Int, [Int])] -> [(Int, [Int])]
processObservations [] _ currentList = currentList
processObservations (observation:observations) state currentList =
  case eventType observation of
    GuardChange ->
      let newState = State {currentGuard = guardId observation, sleptMinute = -1}
      in processObservations observations newState currentList
    Sleep ->
      let newState = state {sleptMinute = thisMinute}
      in processObservations observations newState currentList
    Wake ->
      let sleptMinutes = [(sleptMinute state)..(thisMinute - 1)]
          newList = (currentGuard state, sleptMinutes) : currentList
      in processObservations observations state newList
    where thisMinute = parseMinute (timestamp observation)

parseObservation :: String -> Observation
parseObservation s =
  let [[_, timestamp, rest]] = s =~ "^\\[(.*)\\] (.*)$" :: [[String]]
      eventType =
        case rest of _
                      | rest =~ "begins shift" -> GuardChange
                      | rest =~ "falls asleep" -> Sleep
                      | rest =~ "wakes up" -> Wake
      guardId = if eventType == GuardChange
                then let [[_, gid]] = rest =~ "Guard #([0-9]*)" :: [[String]]
                     in read gid :: Int
                else -1
  in Observation { timestamp = timestamp
                 , eventType = eventType
                 , guardId = guardId
                 }

-- Return the minute portion of a timestamp
parseMinute :: String -> Int
parseMinute timestamp =
  let [[_, minute]] = timestamp =~ ":(..)$" :: [[String]]
  in read minute :: Int

part2 :: [Observation] -> Int
part2 observations =
  let observationMap = guardIdToMinuteArray (sort observations)
      mostFrequentSleepMinute =
        minuteTotals >>> mostFrequentMinuteEntry $ observationMap
      (guard, minuteList) = mostFrequentSleepMinute
      (minute, _) = maximumBy (comparing snd) minuteList
  in guard * minute

-- From a map of guardId to minutes slept, return a map of guardId to array of
-- tuples of (minute, frequency).  There will be 60 elements in the array.
-- TODO do we really need a tuple, or would it suffice to use the list index as
-- the minute?
minuteTotals :: Map Int [Int] -> Map Int [(Int, Int)]
minuteTotals observationMap =
  let minuteCount v = [(m, occurrences (==m) v) | m <- [0..59]]
  in M.map minuteCount observationMap

-- From a map of guardId to array of tuples of (minute, frequency), return the
-- element with the highest number of minutes
mostFrequentMinuteEntry :: Map Int [(Int, Int)] -> (Int, [(Int, Int)])
mostFrequentMinuteEntry minuteTotals =
  let greatest minuteList = snd (maximumBy (comparing snd) minuteList)
      compareEntry (_, arr1) (_, arr2) = compare (greatest arr1) (greatest arr2)
  in maximumBy compareEntry (toList minuteTotals)
