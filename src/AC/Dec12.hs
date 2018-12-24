module AC.Dec12 (
  part1
  , parseInitialState
  , parseRule
  , nextGeneration
  , part2
  ) where

import System.Environment
import Helpers
import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Regex.Posix
import Debug.Trace

main = do
    [problem, filename] <- getArgs
    fileContents <- stringsFromFile filename
    let initialState = parseInitialState $ head fileContents
        rules = M.fromList (map parseRule (drop 2 fileContents))
    putStrLn (case problem of
               "1" -> show (part1 initialState rules)
               "2" -> show (part2 initialState rules))

type Rules = M.Map [Char] Char
-- The numeric value of the first pot represented, and the array of pots.  This
-- is so we don't have to store a bunch of empty entries at the beginning of the
-- array; the array is assumed to start at the index of the first pot.
type PotState = (Int, [Char])

part1 :: [Char] -> Rules -> Int
part1 initialState rules =
  let (firstIdx, finalState) = foldl' (\(firstIdx, pots) _ ->
        nextGeneration rules firstIdx pots) (0, initialState) [1..20]
      numberedPots = zip [firstIdx..] finalState
      -- filter((=='#').snd) is shorter but perhaps less clear
      occupiedPots = filter(\x -> snd x == '#') numberedPots
      numbers = map fst occupiedPots
  in sum numbers

parseInitialState :: String -> [Char]
parseInitialState s =
  let [[_, initialState]] = s =~ "^initial state: (.*)$" :: [[String]]
  in initialState

parseRule :: String -> ([Char], Char)
parseRule s =
  let [[_, rule, outcome]] = s =~ "^(.....) => (.)$" :: [[String]]
  in (rule, head outcome)

-- Calculate the next generation of pots based on the current one.  Arguments:
-- rules, pot number of first array entry, current list of pots.  Return value:
-- tuple with pot number of first array entry, new list of pots
nextGeneration :: Rules -> Int -> [Char] -> PotState
nextGeneration rules firstPot state =
  let paddedState = "...." ++ state ++ "...."
      surroundingPots =
        [ take 5 (drop x paddedState) | x <- [0..(length paddedState - 5)] ]
      newStateFull = map (\p -> rules M.! p) surroundingPots
      firstPlantIdx = fromJust(findIndex (=='#') newStateFull)
      adjAmt = 2 - firstPlantIdx
      newState = drop firstPlantIdx >>> dropWhileEnd (=='.') $ newStateFull
  in (firstPot - adjAmt, newState)

part2 :: [Char] -> Rules -> Int
part2 initialState rules =
  let (cycleStart, cycleLength, history) = findCycle rules (0, initialState) []
      offset = length(history) - fst(history !! (cycleStart - 1))
      firstIdxFiftyBillion = 50000000000 - offset
      numberedPots = zip [firstIdxFiftyBillion..] (snd(history !! cycleStart))
      occupiedPots = filter(\x -> snd x == '#') numberedPots
      numbers = map fst occupiedPots
  in sum numbers

-- Run until a cycle is encountered, then return the cycle length, offset of the
-- cycle start, and all pot values until the cycle was detected.  Arguments:
-- rules, (pot number of first array entry and current list of pots), history.
-- Stolen shamelessly from Day 18.  Could be generalized if the function to run
-- the next generation were a parameter.
findCycle :: Rules -> PotState -> [PotState] -> (Int, Int, [PotState])
findCycle rules (first, state) history =
  case findIndex (\(_, s) -> s == state) history of
    Just firstOcc -> (firstOcc, ((length history) - firstOcc), history)
    otherwise ->
      let (newFirst, newGeneration) = nextGeneration rules first state
      in findCycle rules (newFirst, newGeneration) (history ++ [(first, state)])
