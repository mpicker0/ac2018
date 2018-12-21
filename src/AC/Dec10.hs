module AC.Dec10 (
  part1
  , part2
  , Point (..)
  , parsePoint
  , newPoint
  , findSmallest
  , renderPoints
  ) where

import System.Environment
import Helpers
import Data.List
import qualified Data.Set as Set
import Text.Regex.Posix

main = do
    [problem, filename] <- getArgs
    points <- thingsFromFile filename parsePoint
    putStrLn (case problem of
               "1" -> part1 points
               "2" -> show(part2 points))

data Point = Point { position :: [Int]
                   , velocity :: [Int]
                   } deriving (Eq, Show)

part1 :: [Point] -> String
part1 points =
  let smallest = findSmallest points
      next ps = map newPoint ps
      end = iterate next points !! smallest
  in "\n" ++ renderPoints end

parsePoint :: String -> Point
parsePoint s =
  let [[_, x, y, dx, dy]] =
        s =~ "^position=< *([-0-9]*), *([-0-9]*)> velocity=< *([-0-9]*), *([-0-9]*)>$" :: [[String]]
  in Point [read x :: Int, read y :: Int] [read dx :: Int, read dy :: Int]

-- Find the iteration where the area of the box containing all the points is
-- at its smallest
findSmallest :: [Point] -> Int
findSmallest points = findSmallestRec points 0 maxBound::Int

-- Recursive helper for findSmallest.  Arguments: list of Point, current
-- seconds elapsed, smallest area seen so far.  It is expected that the area
-- will continue to shrink, hit a minimum, then start expanding again; the
-- minimum is what we're after.
findSmallestRec :: [Point] -> Int -> Int -> Int
findSmallestRec points seconds smallestArea =
  let (minx, miny, maxx, maxy) = bounds points
      newPoints = map newPoint points
      newArea = (maxx - minx + 1) * (maxy - miny + 1)
      newSeconds = seconds + 1
  in if (newArea > smallestArea)
     then seconds - 1  -- the previous iteration was smaller
     else findSmallestRec newPoints newSeconds (minimum [newArea, smallestArea])

-- Calculate the min/max coordinates in the list of Point
bounds :: [Point] -> (Int, Int, Int, Int)
bounds points =
  let minx = minimum [x | (Point (x:_) _) <- points]
      miny = minimum [y | (Point (_:y:_) _) <- points]
      maxx = maximum [x | (Point (x:_) _) <- points]
      maxy = maximum [y | (Point (_:y:_) _) <- points]
  in (minx, miny, maxx, maxy)

-- Determine the new position of a Point after a second.  The entire Point
-- is returned, even though the velocity doesn't change.
newPoint :: Point -> Point
newPoint p@(Point [x, y] [dx, dy]) = p { position = [x + dx, y + dy] }

-- Render a list of points into a string that can be printed, similar to how
-- it appears in the examples.
renderPoints :: [Point] -> String
renderPoints points =
  let (minx, miny, maxx, maxy) = bounds points
      offset (x:y:_) = [x - minx, y - miny]
      coordSet = Set.fromList [ offset(position p) | p <- points ]
      display r c = if (Set.member [c, r] coordSet) then '#' else '.'
      getRow r = [ cell | cell <- map (display r) [0..(maxx - minx)] ]
      rows = [ r | r <- map getRow [0..(maxy - miny)] ]
  in intercalate "\n" rows

part2 :: [Point] -> Int
part2 = findSmallest
