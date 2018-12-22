module AC.Dec08 (
  part1
  , Node (..)
  , buildNodeRec
  , buildChildren
  , totalMetadata
  , part2
  ) where

import System.Environment
import Helpers
import Data.List

main = do
    [problem, filename] <- getArgs
    s <- stringsFromFile filename
    let license = map (\w -> read w :: Int) (words (head s))
    putStrLn (case problem of
               "1" -> show(part1 license)
               "2" -> show(part2 license))

data Node = Node { children :: [Node]
                 , metadata :: [Int]
                 } deriving (Eq, Show)
type License = [Int]

-- Given a License, build a tree and return the root node
buildNode :: License -> Node
buildNode license =
  let (node, _) = buildNodeRec license
  in node

-- Recursive helper for buildNode.  Arguments:  License.  Returns a Node and
-- the remaining unconsumed License.
buildNodeRec :: License -> (Node, License)
buildNodeRec (numChildren:numMeta:remaining) =
  let (c, r) = buildChildren remaining numChildren
  in (Node {children = c, metadata = take numMeta r}, drop numMeta r)

-- Build a specified number of children.  Arguments: License, number of children
-- to build; returns child nodes built, remaining unconsumed license
buildChildren :: License -> Int -> ([Node], License)
buildChildren license numChildren =
  iterate (\(ns, l) -> let (n, r) = buildNodeRec l
                       in (ns ++ [n], r)) ([], license) !! numChildren

-- Total the metadata of a Node and all its children
totalMetadata :: Node -> Int
totalMetadata node =
  sum (metadata node) + sum (map totalMetadata (children node))

part1 :: License -> Int
part1 license =
  let root = buildNode license
  in totalMetadata root

part2 :: License -> Int
part2 license = -2
