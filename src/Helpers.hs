module Helpers(intsFromFile, stringsFromFile, occurrences) where

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

-- Count the number of times a predicate in a list is satisfied
-- This composes together the length and filter function; we basically filter
-- the list based on the predicate, then take the length of the result, i.e.
-- length(filter(pred list))
occurrences :: (a -> Bool) -> [a] -> Int
occurrences pred = length . filter pred
