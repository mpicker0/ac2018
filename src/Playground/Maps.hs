module Playground.Maps (frequencyMap, groupByListOfTuple, myGroupBy) where

import qualified Data.Map
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (groupBy, sortBy)
import Control.Arrow

frequencyMap :: (Ord a) => [a] -> Data.Map.Map a Int
frequencyMap l =
  foldl (\acc x -> Data.Map.insertWith (+) x 1 acc) Data.Map.empty l

-- Almost like Scala's groupBy; we end up with a list of tuples but this could
-- easily be converted into a Map
-- Borrowed from:  https://stackoverflow.com/questions/12398458/how-to-group-similar-items-in-a-list-using-haskell
groupByListOfTuple :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupByListOfTuple as =
  sortBy (comparing fst)        -- always sort before groupBy
  >>> groupBy ((==) `on` fst)   -- now we have [[(a, xx), (a, yy)], [(b, xx)]]
  >>> map (\l -> (fst . head $ l, map snd l))
  $ as

myGroupBy :: (Eq a, Ord a) => [(a, b)] -> Data.Map.Map a [b]
myGroupBy as = groupByListOfTuple >>> Data.Map.fromList $ as
