module Playground.LinkedLists (
  DList (..)
  , twoItemDList
  , CList (..)
  , twoItemCList
  , oneItemCList
) where

import qualified Data.Map

-- Builds a 2-item doubly linked list.  Taken from
--  https://www.reddit.com/r/haskell/comments/2nepr0/implementing_doubly_linked_lists_in_haskell/
data DList a = Empty | DCell { value :: a, prev :: DList a, next :: DList a }

-- Since head and tail are computed lazily, we can use one in the call to the
-- other
twoItemDList :: a -> a -> DList a
twoItemDList value1 value2 = head
    where head = DCell value1 Empty tail
          tail = DCell value2 head Empty


-- My modification of the above to make a circular list
data CList a = CCell { cvalue :: a, cprev :: CList a, cnext :: CList a }
twoItemCList :: a -> a -> CList a
twoItemCList value1 value2 = head
    where head = CCell value1 tail tail
          tail = CCell value2 head head

oneItemCList :: a -> CList a
oneItemCList value1 = cell
    where cell = CCell value1 cell cell
