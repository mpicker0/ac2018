module AC.Dec13 (
  part1
  , State (..)
  , Coord (..)
  , Track (..)
  , Cart (..)
  , Direction (..)
  , Action (..)
  , createTrackMap
  , getInitialState
  , moveCart
  , moveCarts
  , moveUntilCollision
  , moveThenRemoveCollision
  , part2
  ) where

import System.Environment
import Helpers
import qualified Data.Map as M
import Control.Arrow
import Data.Ord (comparing)
import Data.List (sort, sortBy, intercalate)

main = do
    [problem, filename] <- getArgs
    rows <- stringsFromFile filename
    let initialState = getInitialState(createTrackMap rows)
    putStrLn (case problem of
               "1" -> part1 initialState
               "1-debug" -> "\n" ++ (part1Debug initialState)
               "2" -> part2 initialState)

data Coord = Coord { x :: Int
                   , y :: Int} deriving (Eq, Show)
instance Ord Coord where
  compare (Coord xa ya) (Coord xb yb)
    | ya == yb = compare xa xb
    | otherwise = compare ya yb

data Direction = N | E | S | W deriving (Eq, Show, Ord, Enum)
-- when facing a certain direction, turn right or left
getRight W = N
getRight d = succ d
getLeft N = W
getLeft d = pred d

-- the action to take at an intersection
data Action = TurnLeft | GoStraight | TurnRight deriving (Eq, Show, Enum)
-- the next action to take at an intersection (just advance through the list)
getNextAction TurnRight = TurnLeft
getNextAction a = succ a

data Cart = Cart { position :: Coord
                 , direction :: Direction
                 , nextAction :: Action } deriving (Eq, Show)

data Track = Horiz | Vert | Cross | SWNE | NWSE
           | CartN | CartS | CartE | CartW
           | Empty deriving (Eq, Ord)
-- Printing and parsing types of Track.  Seems like a lot of work.
instance Show Track where
  show Horiz = "-"
  show Vert = "|"
  show Cross = "+"
  show SWNE = "/"
  show NWSE = "\\"
  show CartN = "^"
  show CartS = "v"
  show CartE = ">"
  show CartW = "<"
  show Empty = " "
instance Read Track where
  readsPrec _ s = case s of
                    "-" -> [(Horiz, "")]
                    "|" -> [(Vert, "")]
                    "+" -> [(Cross, "")]
                    "/" -> [(SWNE, "")]
                    "\\" -> [(NWSE, "")]
                    "^" -> [(CartN, "")]
                    "v" -> [(CartS, "")]
                    ">" -> [(CartE, "")]
                    "<" -> [(CartW, "")]
                    " " -> [(Empty, "")]
                    otherwise -> [(Empty, "")]

type TrackMap = M.Map Coord Track

data State = State { trackMap :: TrackMap
                   , carts :: [Cart]
                   } deriving (Eq, Show)

-- Given the area input as a list of rows (as it appears in the input file),
-- create a map of (x, y) coordinate to type of element
createTrackMap :: [String] -> TrackMap
createTrackMap rows =
  let toTrack c = read [c] :: Track
      getRow y row = [ ((Coord x y), toTrack char)
                       | (x, char) <- zip [0..] row ]
  in M.fromList $ concatMap (\(y, row) -> getRow y row) (zip [0..] rows)

-- Extract the carts from the track map and return the initial state.  Carts
-- aren't technically part of the track layout.
getInitialState :: TrackMap -> State
getInitialState tm =
      -- separate the cart pieces from the rest
  let (carts, rest) =
        M.partitionWithKey (\c t -> t `elem` [CartN, CartS, CartE, CartW]) tm
      -- create the cart list from the cart pieces
      cartDirection t =
        M.fromList [(CartN, N), (CartS, S), (CartE, E), (CartW, W)] M.! t
      cartList = [ Cart c (cartDirection t) TurnLeft
                   | (c, t) <- M.toList carts ]
      -- add back the missing pieces with the correct track type
      mapNoCarts =
        M.map (\v -> if v `elem` [CartN, CartS] then Vert else Horiz) carts
  in State (M.union mapNoCarts rest) cartList

-- helpers
northCart c@(Cart (Coord x y) _ _) = c { position = (Coord x (y - 1)) }
southCart c@(Cart (Coord x y) _ _) = c { position = (Coord x (y + 1)) }
eastCart c@(Cart (Coord x y) _ _) = c { position = (Coord (x + 1) y) }
westCart c@(Cart (Coord x y) _ _) = c { position = (Coord (x - 1) y) }

turnCart (Cart p d action) =
  let newDirection = case action of
                       TurnLeft -> getLeft d
                       TurnRight -> getRight d
                       otherwise -> d
      newAction = getNextAction action
  in Cart p newDirection newAction

moveCart :: TrackMap -> Cart -> Cart
moveCart tm c@(Cart p od _) =
  let track = tm M.! p
      -- determine new orientation/action
      (Cart _ d a) =
        case track of
          SWNE -> case od of _
                              | od `elem` [N, S] -> c {direction = getRight od}
                              | od `elem` [E, W] -> c {direction = getLeft od}
          NWSE -> case od of _
                              | od `elem` [N, S] -> c {direction = getLeft od}
                              | od `elem` [E, W] -> c {direction = getRight od}
          Cross -> turnCart c
          otherwise -> c
      -- move
      newCart = case d of
                  N -> northCart (Cart p d a)
                  S -> southCart (Cart p d a)
                  E -> eastCart (Cart p d a)
                  W -> westCart (Cart p d a)
  in newCart

-- Given the current state, determine the next state
moveCarts :: State -> State
moveCarts (State tm carts) =
  let sortedCarts cs = sortBy (comparing position) cs
      isVictim cs (Cart pos _ _) = any (\(Cart x _ _) -> x == pos) cs
      -- if this cart has been previously hit by another cart, don't move it
      newPos cs c = if (isVictim cs c) then c else moveCart tm c
      newCarts = foldl (\acc c -> acc ++ [newPos acc c]) [] (sortedCarts carts)
  in State tm newCarts

moveUntilCollision :: State -> State
moveUntilCollision initialState =
  let collisionExists carts =
        any (>1) (M.elems (frequencyMap [p | (Cart p _ _) <- carts]))
      collidedState =
        until (\(State _ carts) -> collisionExists carts) moveCarts initialState
  in collidedState

part1 :: State -> String
part1 initialState =
  let (State _ carts) = moveUntilCollision initialState
      freqMap = frequencyMap [p | (Cart p _ _) <- carts]
      collisions = M.filter (>1) freqMap
      ((Coord x y), _) = head (M.toList collisions)
  in show x ++ "," ++ show y

-- START DEBUG STUFF
-- For debugging/exploration
-- Determine the right-bottom corner in the list of coordinates
farCorner :: [Coord] -> Coord
farCorner coords =
  let right = maximum [x | (Coord x _) <- coords]
      bottom = maximum [y | (Coord _ y) <- coords]
  in (Coord right bottom)

-- Attach the carts to the map so it's easier to visualize
attachCarts :: [Cart] -> TrackMap -> TrackMap
attachCarts carts tm =
  let cartIcon dir =
        M.fromList [(N, CartN), (S, CartS), (E, CartE), (W, CartW)] M.! dir
  in foldl (\acc (Cart p d _) -> M.insert p (cartIcon d) acc) tm carts

-- Render the area map into a string that can be printed, similar to how it
-- appears in the examples.
renderTrack :: TrackMap -> String
renderTrack tm =
   let (Coord right bottom) = farCorner (M.keys tm)
       getRow y =
         [ show(M.findWithDefault Empty (Coord x y) tm) | x <- [0..right] ]
       rows = [ concat(getRow y) | y <- [0..bottom] ]
   in intercalate "\n" rows

part1Debug :: State -> String
part1Debug state =
  let stateToShow = iterate moveCarts state !! 1
      (State trackMap carts) = stateToShow
  in (attachCarts carts >>> renderTrack $ trackMap) ++ "\n" ++ show carts
-- END DEBUG STUFF

moveThenRemoveCollision :: State -> State
moveThenRemoveCollision state =
  let (State tm carts) = moveUntilCollision state
      freqMap = frequencyMap [p | (Cart p _ _) <- carts]
      collisions = M.filter (>1) freqMap
      uncollidedCarts = filter (\(Cart p _ _) -> M.notMember p collisions) carts
  in State tm uncollidedCarts

moveUntilOneCart :: State -> State
moveUntilOneCart s@(State _ carts) =
  let lastCartState =
        until (\(State _ carts) -> length carts == 1) moveThenRemoveCollision s
  in lastCartState

part2 :: State -> String
part2 initialState =
  let (State _ carts) = moveUntilOneCart initialState
      (Cart (Coord x y) _ _) = head carts
  in show x ++ "," ++ show y
