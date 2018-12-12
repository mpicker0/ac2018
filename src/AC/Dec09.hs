module AC.Dec09 (
  part1
  , State (..)
  , initialState
  , playTurn
  , placeMarble
  , removeMarble
  , addToIndex
  ) where

-- LOTS of opportunities to improve this; it works but is very slow, probably
-- because it's spending so much time splitting the list to do inserts.  Part 2
-- would take days to run at this rate.  Also, the output only prints when I
-- enable debugging.  I suspect the putStrLn is running too quickly, or the
-- scores array isn't available, or some other weird thing.  The tests pass at
-- least.

import System.Environment
import Helpers
import Data.List
import Data.Maybe (fromJust)
import Text.Regex.Posix
-- TODO remove
import Debug.Trace

main = do
    [problem, filename] <- getArgs
    gameConfigs <- thingsFromFile filename parseGameConfig
    let gameConfig = head gameConfigs
    putStrLn (case problem of
               "1" -> show (part1 gameConfig)
               "2" -> show (part2 gameConfig))

data State = State { circle :: [Int]
                   , currentMarbleIdx :: Int
                   , scores :: [Int]
                   } deriving (Eq, Show)

initialState players =
  State { circle = [0]
        , currentMarbleIdx = 0
        , scores = replicate players 0
        }

part1 :: (Int, Int) -> Int
part1 (players, lastMarble) =
  let state = initialState players
      scores = playUntil state players lastMarble
  in maximum scores

-- Parse the description into a tuple of (players, last marble)
parseGameConfig :: String -> (Int, Int)
parseGameConfig s =
  let [[_, players, lastMarble]] = s =~ "^([0-9]*) players; last marble is worth ([0-9]*) points$" :: [[String]]
  in (read players :: Int, read lastMarble :: Int)

-- Play until the last marble has been placed and return an array of scores
-- Arguments:  State, players, last marble
playUntil :: State -> Int -> Int -> [Int]
playUntil state players lastMarble =
  let playersAndMarbles = (zip (cycle [0..players - 1]) [1..lastMarble])
      finalState = foldl' (\acc (player, marbleToPlace) ->
        playTurn acc marbleToPlace player) state playersAndMarbles
  in scores finalState

-- Place a marble into the circle and return the index of the new current marble
-- and updated circle.  Arguments:  value of marble to place, current marble
-- index, current circle
placeMarble :: Int -> Int -> [Int] -> (Int, [Int])
placeMarble newMarble curIdx circle =
  let
      -- determine index of the marble to insert after
      insertIdx = (curIdx + 2) `mod` (length circle)
      (first, second) = splitAt insertIdx circle
  in (insertIdx, first ++ [newMarble] ++ second)

-- Remove a marble from the circle and return the new current marble index,
-- value of the marble removed, and the updated circle.  Arguments: current
-- marble index, current circle
removeMarble :: Int -> [Int] -> (Int, Int, [Int])
removeMarble curIdx circle =
  let
      -- determine the index of the marble to remove
      removeIdx = (curIdx - 7) `mod` (length circle)
      -- determine the value of the marble removed for scoring
      remove = circle !! removeIdx
      newCurIdx = removeIdx `mod` (length circle)
  in (newCurIdx, remove, delete remove circle)

-- Add a given integer to the specified element in the list.  Arguments:  the
-- index, the amount to add, the input list
addToIndex :: Int -> Int -> [Int] -> [Int]
addToIndex idx amt ints =
  let (h, i:t) = splitAt idx ints
  in h ++ [i + amt] ++ t

-- Play a turn and return the new game state.  Arguments are the current state,
-- the marble to place, and the player number.
playTurn :: State -> Int -> Int -> State
-- TODO remove debugging
--playTurn state marbleToPlace player | trace("playing turn: " ++ show (marbleToPlace) ++ "; current high: " ++ show (maximum (scores state)) )  False = undefined
playTurn state marbleToPlace player =
  -- TODO is this the best way to extract everything?
  let (State circle' currentMarbleIdx' scores') = state
      -- determine the new circle and current marble based on whether this
      -- marble is a multiple of 23
      (newCurrentMarbleIdx, newCircle, scored) = if (marbleToPlace `rem` 23 == 0)
        then
          let (newCur, marbleRemoved, newCircle) = removeMarble currentMarbleIdx' circle'
          in (newCur, newCircle, marbleToPlace + marbleRemoved)
        else
          let (newCur, newCircle) = placeMarble marbleToPlace currentMarbleIdx' circle'
          in (newCur, newCircle, 0)
      newScores = addToIndex player scored scores'
  in State newCircle newCurrentMarbleIdx newScores

-- TODO this takes far too long; is there any way to shorten the list, or do we
-- potentially need older marbles?
part2 :: (Int, Int) -> Int
part2 (players, lastMarble) =
  let state = initialState players
      scores = playUntil state players (lastMarble * 100)
  in maximum scores
