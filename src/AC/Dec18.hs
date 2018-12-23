module AC.Dec18 (
  part1
  , part1Debug
  , Coord (..)
  , Acre (..)
  , createCoordMap
  , renderArea
  , surroundingCounts
  , acreNewState
  , newState
  , part2
  ) where

import System.Environment
import Helpers
import Data.List
import qualified Data.Map as M
import Data.Maybe (catMaybes)

main = do
    [problem, filename] <- getArgs
    rows <- stringsFromFile filename
    putStrLn (case problem of
               "1" -> show(part1 rows)
               "1-debug" -> "\n" ++ (part1Debug rows)
               "2" -> show(part2 rows))

part1 :: [String] -> Int
part1 rows =
  let coordMap = createCoordMap rows
      finalState = iterate newState coordMap !! 10
      count acreType = occurrences (==acreType) (M.elems finalState)
  in count Wooded * count Lumberyard

-- For debugging/exploration
part1Debug :: [String] -> String
part1Debug rows =
  let coordMap = createCoordMap rows
      runTimes n = iterate newState coordMap !! n
      renderings = [ (show n) ++ "\n" ++ renderArea (runTimes n) | n <- [0..50] ]
  in intercalate "\n\n" renderings

data Coord = Coord { x :: Int
                   , y :: Int} deriving (Eq, Show, Ord)

data Acre = Open | Wooded | Lumberyard deriving (Eq)
-- Printing and parsing types of Acres.  Seems like a lot of work.
instance Show Acre where
  show Open = "."
  show Wooded = "|"
  show Lumberyard = "#"
instance Read Acre where
  readsPrec _ s = case s of
                    "." -> [(Open, "")]
                    "|" -> [(Wooded, "")]
                    "#" -> [(Lumberyard, "")]
                    otherwise -> []

type AreaMap = M.Map Coord Acre

-- Given the area input as a list of rows (as it appears in the input file),
-- create a map of (x, y) coordinate to Acre (the type of square)
createCoordMap :: [String] -> AreaMap
createCoordMap rows =
  let toAcre c = read [c] :: Acre
      getRow y row = [ ((Coord x y), toAcre char) | (x, char) <- zip [0..] row ]
  in M.fromList $ concatMap (\(y, row) -> getRow y row) (zip [0..] rows)

-- Determine the right-bottom corner in the list of coordinates
farCorner :: [Coord] -> Coord
farCorner coords =
  let right = maximum [x | (Coord x _) <- coords]
      bottom = maximum [y | (Coord _ y) <- coords]
  in (Coord right bottom)

-- Render the area map into a string that can be printed, similar to how it
-- appears in the examples.
renderArea :: AreaMap -> String
renderArea area =
   let (Coord right bottom) = farCorner (M.keys area)
       getRow y = [ show(area M.! (Coord x y)) | x <- [0..right] ]
       rows = [ concat(getRow y) | y <- [0..bottom] ]
   in intercalate "\n" rows

-- Returns the coordinates of all the areas surrounding this one
surroundingCoords :: Coord -> [Coord]
surroundingCoords (Coord cx cy) =
  [Coord x y | x <- [cx-1..cx+1], y <- [cy-1..cy+1], (x, y) /= (cx, cy) ]

-- Given an area map and coordinate, count the number of open areas, wooded
-- areas, and lumberyards that surround it.
surroundingCounts :: AreaMap -> Coord -> (Int, Int, Int)
surroundingCounts areaMap coord =
  let surroundingItems =
        catMaybes [areaMap M.!? a | a <- surroundingCoords coord]
      count acreType = occurrences (==acreType) surroundingItems
  in (count Open, count Wooded, count Lumberyard)

-- Given an area map and coordinate, determine the new state of the acre at the
-- coordinate
acreNewState :: AreaMap -> Coord -> Acre
acreNewState areaMap coord =
  let (open, wooded, lumberyard) = surroundingCounts areaMap coord
  in case areaMap M.! coord of
       Open -> if wooded >= 3 then Wooded else Open
       Wooded -> if lumberyard >= 3 then Lumberyard else Wooded
       Lumberyard -> if lumberyard > 0 && wooded > 0 then Lumberyard else Open

-- Calculate the new state of the map after a minute
newState :: AreaMap -> AreaMap
newState areaMap = M.mapWithKey (\k _ -> acreNewState areaMap k) areaMap

part2 :: [String] -> Int
part2 rows =
  let coordMap = createCoordMap rows
      (cycleStart, cycleLength, history) = findCycle coordMap []
      timeBillion = ((1000000000 - cycleStart) `mod` cycleLength) + cycleStart
      area = history !! timeBillion
      count acreType = occurrences (==acreType) (M.elems area)
  in count Wooded * count Lumberyard

-- Run until a cycle is encountered, then return the cycle length, offset of the
-- cycle start, and all area values until the cycle was detected
findCycle :: AreaMap -> [AreaMap] -> (Int, Int, [AreaMap])
findCycle areaMap history =
  case findIndex (==areaMap) history of
    Just firstOcc -> (firstOcc, ((length history) - firstOcc), history)
    otherwise -> findCycle (newState areaMap) (history ++ [areaMap])
