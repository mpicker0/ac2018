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

main = do
    [problem, filename] <- getArgs
    fileContents <- stringsFromFile filename
    let initialState = parseInitialState $ head fileContents
        rules = M.fromList (map parseRule (drop 2 fileContents))
    putStrLn (case problem of
               "1" -> show (part1 initialState rules)
               "2" -> show (part2 initialState rules))

part1 :: [Char] -> M.Map [Char] Char -> Int
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
nextGeneration :: M.Map [Char] Char -> Int -> [Char] -> (Int, [Char])
nextGeneration rules firstPot state =
  let paddedState = "...." ++ state ++ "...."
      surroundingPots =
        [ take 5 (drop x paddedState) | x <- [0..(length paddedState - 5)] ]
      newStateFull = map (\p -> rules M.! p) surroundingPots
      firstPlantIdx = fromJust(findIndex (=='#') newStateFull)
      adjAmt = 2 - firstPlantIdx
      newState = drop firstPlantIdx >>> dropWhileEnd (=='.') $ newStateFull
  in (firstPot - adjAmt, newState)

part2 :: [Char] -> M.Map [Char] Char -> Int
part2 initialState rules = -2
