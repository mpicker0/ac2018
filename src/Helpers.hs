module Helpers(intsFromFile) where

-- TODO generalize to Num?
intsFromFile :: String -> IO [Int]
intsFromFile filename = do
  l <- readFile filename
  let fileLines = lines l
  let mappedLines = map(\l -> read (filter (/='+') l) :: Int) fileLines
  return mappedLines
