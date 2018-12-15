module AC.Dec07 (
  part1
  , part2
  , StepReq (..)
  , State (..)
  , parseStepReq
  , buildInitialState
  , completeStep
  ) where

import System.Environment
import Helpers
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as M
import Text.Regex.Posix

main = do
    [problem, filename] <- getArgs
    stepReqs <- thingsFromFile filename parseStepReq
    putStrLn (case problem of
               "1" -> part1 stepReqs
               "2" -> part2 stepReqs)

data StepReq = StepReq { pre :: String
                       , post :: String
                       } deriving (Eq, Show)

data State = State { prereqMap :: M.Map String [String]
                   , candidates :: [String]
                   , finished :: [String]
                   } deriving (Eq, Show)

part1 :: [StepReq] -> String
part1 reqs =
  let openingState = buildInitialState reqs
      finalList = processUntilFinished openingState
  in concat finalList

-- Parse the step requirements
parseStepReq :: String -> StepReq
parseStepReq s =
  let [[_, pre, post]] =
        s =~ "^Step (.) must be finished before step (.)" :: [[String]]
  in StepReq { pre = pre, post = post }

-- Build the initial state from the list of step requirements
buildInitialState :: [StepReq] -> State
buildInitialState stepReqs =
  let allPre = Set.fromList (map pre stepReqs)
      allPost = Set.fromList (map post stepReqs)
      reqMap = foldl (\acc sr ->
        M.insertWith (++) (post sr) [pre sr] acc) M.empty stepReqs
      initCandidates = Set.toList(Set.difference allPre allPost)
  in State reqMap initCandidates []

-- From the initial state, keep processing until finished (no steps remaining)
-- and return the final order
processUntilFinished :: State -> [String]
processUntilFinished (State _ [] finished) = finished
processUntilFinished state = processUntilFinished (completeStep state)

-- Complete the next step to be completed and return the new state
completeStep :: State -> State
completeStep (State prereqMap (selected:remaining) finished) =
  let mapAfterWorking = M.map (delete selected) prereqMap
      (availableTasks, unavailableTasks) = M.partition(==[]) mapAfterWorking
      newCandidates = Data.List.sort(remaining ++ M.keys availableTasks)
      newFinished = finished ++ [selected]
  in State unavailableTasks newCandidates newFinished

part2 :: [StepReq] -> String
part2 _ = "TODO 2"
