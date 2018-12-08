module Helpers
  ( intsFromFile
  , stringsFromFile
  , thingsFromFile
  , occurrences
  , frequencyMap
  ) where

import Data.Map (Map, empty, insertWith)

-- TODO generalize to Num?
intsFromFile :: String -> IO [Int]
intsFromFile filename = do
  l <- readFile filename
  let fileLines = lines l
  let mappedLines = map(\l -> read (filter (/='+') l) :: Int) fileLines
  return mappedLines

stringsFromFile :: String -> IO [String]
stringsFromFile filename = do
  l <- readFile filename
  return (lines l)

-- Generic function to read "things" from a file and return a list.  A converter
-- function is supplied that can convert from a String to the thing desired.
thingsFromFile :: String -> (String -> a) -> IO [a]
thingsFromFile filename converter = do
  l <- readFile filename
  let fileLines = lines l
  return (map converter fileLines)

-- Count the number of times a predicate in a list is satisfied
-- This composes together the length and filter function; we basically filter
-- the list based on the predicate, then take the length of the result, i.e.
-- length(filter(pred list))
occurrences :: (a -> Bool) -> [a] -> Int
occurrences pred = length . filter pred

-- Given a list where items may be duplicated, return a Map where the key is an
-- individual item in the list and the value is the number of times it occurs
frequencyMap :: (Ord a) => [a] -> Data.Map.Map a Int
frequencyMap = foldl (\acc x -> Data.Map.insertWith (+) x 1 acc) empty
