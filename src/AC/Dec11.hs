module AC.Dec11 (
  part1
  , Coord (..)
  , determinePower
  , maxPower
  , maxPower2
  , part2
  ) where

import System.Environment
import Helpers
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Ord (comparing)

main = do
    [problem, filename] <- getArgs
    fileContents <- stringsFromFile filename
    let serialNumber = read (head fileContents) :: Int
    putStrLn (case problem of
               "1" -> part1 serialNumber
               "2" -> part2 serialNumber)

gridSize = 300

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)

part1 :: Int -> String
part1 serialNumber =
  let (_, (Coord x y)) = maxPower serialNumber
  in show(x) ++ "," ++ show(y)

-- Given a coordinate and serial number, determine the cell's power level
determinePower :: Coord -> Int -> Int
determinePower (Coord x y) serialNumber =
  let rackId = x + 10
      powerFull = ((rackId * y) + serialNumber) * rackId
      hundredsDigit = powerFull `div` 100 `mod` 10
  in hundredsDigit - 5

-- Given a serial number, determine the maximum total power level of all 3x3
-- squares in the grid.  Returns a tuple of (power level, upper-left coordinate)
maxPower :: Int -> (Int, Coord)
maxPower serial =
  let grid = M.fromList [ (Coord x y, determinePower (Coord x y) serial)
                          | x <- [1..gridSize],
                            y <- [1..gridSize]
                        ]
      -- return a 3x3 square with the upper-left at x, y
      squareFrom (Coord x y) = [(Coord x y) | x <- [x..x + 2], y <- [y..y + 2]]
      -- determine the total power of cells
      squarePower cells = sum [ grid M.! c | c <- cells ]
      allSquaresWithPower = [ (squarePower(squareFrom (Coord x y)), (Coord x y))
                              | x <- [1..gridSize-2],
                                y <- [1..gridSize-2]
                            ]
  in maximumBy (comparing fst) allSquaresWithPower

part2 :: Int -> String
part2 serialNumber =
  let (_, (Coord x y), size) = maxPower2 serialNumber
  in show(x) ++ "," ++ show(y) ++ "," ++ show(size)

-- I'm assuming this would work eventually, but it's far too slow.  Need to
-- find a way to avoid summing up squares multiple times.
maxPower2 :: Int -> (Int, Coord, Int)
maxPower2 serial =
  let grid = M.fromList [ (Coord x y, determinePower (Coord x y) serial)
                          | x <- [1..gridSize],
                            y <- [1..gridSize]
                        ]
      -- return an nxn square with the upper-left at x, y
      squareFrom (Coord x y) n = [ (Coord x y)
                                   | x <- [x..x + n-1],
                                     y <- [y..y + n-1]
                                 ]
      -- determine the total power of cells
      squarePower cells = sum [ grid M.! c | c <- cells ]
      allSquaresWithPower n =
        [ (squarePower(squareFrom (Coord x y) n), (Coord x y))
          | x <- [1..gridSize-n+1],
            y <- [1..gridSize-n+1]
        ]
      -- maximum for a given square size
      maxSquareWithPower n =
        let (p, c) = maximumBy (comparing fst) (allSquaresWithPower n)
        in (p, c, n)
      allMaxSquaresWithPower = map maxSquareWithPower [1..gridSize]
      powerCompareFn (powa, _, _) (powb, _, _) = compare powa powb
  in maximumBy powerCompareFn allMaxSquaresWithPower
