module AC.Dec01(part1, part2) where

import System.Environment
import Helpers
import qualified Data.Set as Set

main = do
    [problem, filename] <- getArgs
    changes <- intsFromFile filename
    print (case problem of
            "1" -> part1 changes
            "2" -> part2 changes)

part1 :: [Int] -> Int
part1 changes = sum changes

part2 :: [Int] -> Int
part2 changes = findRepeatedFrequency (cycle changes) 0 Set.empty

-- TODO do we really need Num a, Ord a?  Is there a better way to express this?
findRepeatedFrequency :: (Num a, Ord a) => [a] -> a -> Set.Set a -> a
findRepeatedFrequency (change:changes) freq seen
    | Set.member (freq + change) seen = freq + change
    | otherwise = findRepeatedFrequency changes (freq + change) (Set.insert freq seen)
findRepeatedFrequency _ _ _ = error "list must contain at least two elements"
