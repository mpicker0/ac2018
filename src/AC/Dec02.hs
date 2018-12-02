module AC.Dec02(part1, part2, analyzeId, dropNth, variations, similar) where

import System.Environment
import Helpers
import Data.List
import qualified Data.Set as Set

main = do
    [problem, filename] <- getArgs
    boxIds <- stringsFromFile filename
    putStrLn (case problem of
               "1" -> show (part1 boxIds)
               "2" -> part2 boxIds)

part1 :: [String] -> Int
part1 boxIds =
  let analyzed = map analyzeId boxIds
      twos = occurrences ((==True).fst) analyzed
      threes = occurrences ((==True).snd) analyzed
  in twos * threes

-- Return a tuple; the first item means a letter occurred exactly twice, the
-- second means a letter occurred exactly three times
analyzeId:: String -> (Bool, Bool)
analyzeId boxId =
  let individuals = Data.List.group (sort boxId)    -- TODO alternative to sort?
      lengthOf len s = length s == len
      twos = any(lengthOf 2) individuals
      threes = any(lengthOf 3) individuals
  in (twos, threes)

part2 :: [String] -> String
part2 boxIds =
  let allPairs = pairs boxIds
      similarList = concatMap (\p -> similar (fst p) (snd p)) allPairs
  in head similarList

-- Borrowed from https://stackoverflow.com/questions/28191103/iterate-over-all-pair-combinations-without-repetition-in-haskell
pairs :: [a] -> [(a, a)]
pairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]

-- Return a string without the nth character
dropNth :: [a] -> Int -> [a]
dropNth list n = [ fst x | x <- zip list [0..], snd x /= n ]

-- Return all variations of a string with one character missing
variations :: String -> [String]
variations s =
  let len = length s
  in [ dropNth s x | x <- [0..len - 1] ]

-- Return a list of similar substrings between two strings; will contain one
-- item if they are similar according to the question rules, no items otherwise
similar :: String -> String -> [String]
similar s1 s2 =
  let pairs = zip (variations s1) (variations s2)
      matches = filter(\p -> fst p == snd p) pairs
  in [ fst x | x <- matches, length matches == 1]
