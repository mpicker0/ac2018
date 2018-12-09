module AC.Dec05 (
  part1,
  part2,
  collapse,
  removeInstances) where

import System.Environment
import Helpers
import Control.Arrow
import Data.Char (toUpper)
import Data.List

main = do
    [problem, filename] <- getArgs
    polymers <- stringsFromFile filename
    let polymer = head polymers
    putStrLn (case problem of
               "1" -> show (part1 polymer)
               "2" -> show (part2 polymer))

part1 :: String -> Int
part1 polymer = length (collapse polymer)

collapse :: String -> String
collapse polymer = collapseRec polymer "" False

-- Arguments: remaining string to scan, current output, removed flag (whether
-- we removed a pair this pass and will need to re-scan)
collapseRec :: String -> String -> Bool -> String
-- finished; no remaining input and we didn't remove any pairs last pass
collapseRec "" output False = output
-- no remaining input but we removed a pair last time and may have further pairs
collapseRec "" output True = collapseRec output "" False
-- recursive case
collapseRec (p1:p2:remaining) output rescan
  | toUpper p1 == toUpper p2 && p1 /= p2 = collapseRec remaining output True
  | remaining == "" = collapseRec "" (output ++ [p1, p2]) rescan
  | otherwise = collapseRec (p2:remaining) (output ++ [p1]) rescan
-- at the end with just one character left
collapseRec (p1:[]) output rescan = collapseRec (output ++ [p1]) "" rescan

-- TODO alternate approach that may run faster:  try to use Data.Text.replace
-- to replace pairs of characters

part2 :: String -> Int
part2 polymer =
  let withoutUnit u = removeInstances u >>> collapse >>> length $ polymer
      allLengths = map withoutUnit ['a'..'z']
  in minimum allLengths

-- Remove all instances of a character from a string
removeInstances :: Char -> String -> String
removeInstances c s = filter (\x -> toUpper x /= toUpper c) s
