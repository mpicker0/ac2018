module AC.Dec03(
  part1,
  part2,
  Claim (..),
  Coord (..),
  parseClaim,
  coveredCoords,
  hasOneClaim) where

import System.Environment
import Helpers
import Data.List
import Data.Map hiding (foldl, map, filter)
import Data.Maybe (fromJust)
import Text.Regex.Posix

main = do
    [problem, filename] <- getArgs
    claims <- thingsFromFile filename parseClaim
    putStrLn (case problem of
               "1" -> show (part1 claims)
               "2" -> show (part2 claims))

data Coord = Coord { x :: Int, y :: Int } deriving (Eq, Ord, Show)
data Claim = Claim { claimId :: Int
                   , coord :: Coord
                   , width :: Int
                   , height :: Int
                   } deriving (Eq, Show)

part1 :: [Claim] -> Int
part1 claims =
  let coordMap = frequencyMap (concatMap coveredCoords claims)
      overlaps = occurrences (\(_, c) -> c > 1) (toList coordMap)
  in overlaps

parseClaim :: String -> Claim
parseClaim s =
  let [[_, cid, x, y, w, h]] = s =~ "^#(.*) @ (.*),(.*): (.*)x(.*$)" :: [[String]]
  in Claim { claimId = read cid :: Int
           , coord = Coord { x = read x :: Int, y = read y :: Int }
           , width = read w :: Int
           , height = read h :: Int
           }

-- Return a list of Coord covered by this Claim, starting from the top left
coveredCoords :: Claim -> [Coord]
coveredCoords c =
  let rightBound = (x.coord) c + width c - 1
      lowerBound = (y.coord) c + height c - 1
  in [ Coord {x = x, y = y} | x <- [(x.coord) c..rightBound],
                              y <- [(y.coord) c..lowerBound] ]

part2 :: [Claim] -> Int
part2 claims =
  let allCoveredCoords = map (\c -> (claimId c, coveredCoords c)) claims
      allCoordinatesAndClaims =
        concatMap (\(cid, coords) ->
          map (\coord -> (coord, [cid])) coords
        ) allCoveredCoords
      coordToClaimIdMap = fromListWith (++) allCoordinatesAndClaims
      allHaveOneClaim (_, coords) = all(hasOneClaim coordToClaimIdMap) coords
      result = find allHaveOneClaim allCoveredCoords
  in fst (fromJust result)

-- True if a coordinate exists in the map and has 1 claim; False if it does not
-- exist or has more/less than one
hasOneClaim :: Data.Map.Map Coord [Int] -> Coord -> Bool
hasOneClaim coordToClaimIds coord =
  let maybeClaimList = Data.Map.lookup coord coordToClaimIds
  in maybe False id (fmap ((1==).length) maybeClaimList)
