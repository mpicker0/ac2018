-- This is just a convenient place to do ad-hoc tests

module TestIt where

import Helpers

main :: IO ()
main = do
  -- l <- readFile "test/data/numbers.txt"
  -- let fileLines = lines l
  -- let mappedLines = map(\l -> read l :: Integer) fileLines
  -- print mappedLines
  l <- intsFromFile "test/data/numbers.txt"
  print(map(+1) l)
  print l
  print (sum l)


-- main = putStrLn "Test something"
