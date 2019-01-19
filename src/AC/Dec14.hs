module AC.Dec14 (
  part1
  , State (..)
  , initialState
  , getNewState
  , tenAfter
  , part2
  , lastn
  , leftOf
  ) where

import System.Environment
import Helpers
import Data.List hiding (insert)
import Data.IntMap hiding (map)
import Control.Arrow

main = do
    [problem, filename] <- getArgs
    fileContents <- stringsFromFile filename
    let recipeCount = read (head fileContents) :: Int
    putStrLn (case problem of
               "1" -> part1 recipeCount
               "2" -> show(part2 recipeCount))

type Scoreboard = IntMap Int

data State = State { elf1Idx :: Int
                   , elf2Idx :: Int
                   , scoreboard :: Scoreboard
                   , nextIdx :: Int -- avoid repeated "length scoreboard" calls
                   } deriving (Eq, Show)

initialState = State { elf1Idx = 0
                     , elf2Idx = 1
                     , scoreboard = toIntMap [3, 7]
                     , nextIdx = 2
                     }

-- Given the current state, determine the next state
getNewState :: State -> State
getNewState (State elf1Idx elf2Idx scoreboard nextIdx) =
  let elf1Score = scoreboard ! elf1Idx
      elf2Score = scoreboard ! elf2Idx
      newScore = elf1Score + elf2Score
      (newScoreboard, newNextIdx) =
        if newScore > 9
          then (insert nextIdx 1
                >>> insert (nextIdx + 1) (newScore - 10)
                $ scoreboard, nextIdx + 2)
          else (insert nextIdx newScore scoreboard , nextIdx + 1)
      newElf1Idx = (elf1Idx + 1 + elf1Score) `mod` newNextIdx
      newElf2Idx = (elf2Idx + 1 + elf2Score) `mod` newNextIdx
  in (State newElf1Idx newElf2Idx newScoreboard newNextIdx)

-- Get the scores of the ten receipes after a given number.  Arguments:  number
-- of recipes, state.
tenAfter :: Int -> State -> String
-- need to compute at least 10 recipes beyond what they asked for
tenAfter n state =
  let laterState = iterate' getNewState initialState !! (10 + n)
      sb = scoreboard laterState
      digits = [ sb ! d | d <- [n..n+9] ]
  in concat(map show digits)

part1 :: Int -> String
part1 recipeCount = tenAfter recipeCount initialState

-- Return a string which is the last n scores from the scoreboard.  Arguments:
-- number of scores, state.
lastn :: Int -> State -> String
lastn n (State _ _ scoreboard nextIdx) =
  if n > nextIdx then ""
  else concatMap show [ scoreboard ! n | n <- [nextIdx - n..nextIdx - 1] ]

-- Return the number of scores to the left of the given string.  Arguments:
-- string to search for, state.
leftOf :: String -> State -> Int
leftOf string state =
  let scoreLen = length string
      lastState = until (\state -> lastn scoreLen state == string)
                        (\st -> getNewState st) state
      -- TODO should probably use this form, in case the last iteration
      -- generated two recipes
      --lastState = until (\state -> isInfixOf string (lastn scoreLen+1 state)) (\st -> getNewState st) state
  in (nextIdx lastState) - scoreLen

-- Runs out of memory somewhere between 20M and 25M :(
part2 :: Int -> Int
part2 recipeCount =
  let recipeStr = show recipeCount
  in leftOf recipeStr initialState
