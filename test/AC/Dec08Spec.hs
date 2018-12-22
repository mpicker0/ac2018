module AC.Dec08Spec (spec) where

import Test.Hspec
import AC.Dec08
import Helpers

spec :: Spec
spec = do

  let nodeD = Node { children = [], metadata = [99]}
      nodeC = Node { children = [nodeD], metadata = [2]}
      nodeB = Node { children = [], metadata = [10, 11, 12]}
      nodeA = Node { children = [nodeB, nodeC], metadata = [1, 1, 2]}
      emptyNode = Node { children = [], metadata = [] }

  describe "part1" $ do
    it "solves the example" $ do
      s <- stringsFromFile "test/data/dec08_input.txt"
      let license = map (\w -> read w :: Int) (words (head s))
      part1 license `shouldBe` 138

  describe "buildChildren" $ do
    it "builds two children" $ do
      let license = [0, 0, 0, 0]
          emptyNode = Node { children = [], metadata = [] }
      buildChildren license 2 `shouldBe` ([emptyNode, emptyNode], [])

    it "builds two children with metadata" $ do
      let license = [0, 1, 1, 0, 2, 2, 2]
          child1 = Node { children = [], metadata = [1] }
          child2 = Node { children = [], metadata = [2, 2] }
      buildChildren license 2 `shouldBe` ([child1, child2], [])

    it "builds a child and returns the remainder of the license" $ do
      let license = [0, 0, 998, 999]
          emptyNode = Node { children = [], metadata = [] }
      buildChildren license 1 `shouldBe` ([emptyNode], [998, 999])

  describe "buildNodeRec" $ do
    it "builds a node and returns the remainder of the license" $ do
      let license = [0, 0, 999]
          emptyNode = Node { children = [], metadata = [] }
      buildNodeRec license `shouldBe` (emptyNode, [999])

    it "builds a node with metadata and returns the remainder" $ do
      let license = [0, 2, 1, 2, 999]
          emptyNode = Node { children = [], metadata = [1, 2] }
      buildNodeRec license `shouldBe` (emptyNode, [999])

    it "builds a node with no children" $ do
      let input = [0, 1, 99]
      buildNodeRec input `shouldBe` (nodeD, [])

    it "builds a node with one child" $ do
      let license = [1, 1, 0, 1, 99, 2]
      buildNodeRec license `shouldBe` (nodeC, [])

    it "builds a node with two children" $ do
      let license = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
      buildNodeRec license `shouldBe` (nodeA, [])

  describe "totalMetadata" $ do
    it "totals the metadata of am empty node" $ do
      totalMetadata emptyNode `shouldBe` 0

    it "totals the metadata of a single node" $ do
      totalMetadata nodeD `shouldBe` 99

    it "totals the metadata in the example" $ do
      totalMetadata nodeA `shouldBe` 138

  describe "totalValue" $ do
    it "totals the value of a node with no children" $ do
      totalValue nodeB `shouldBe` 33
      totalValue nodeD `shouldBe` 99

    it "totals the value of a node with a nonexistent child reference" $ do
      totalValue nodeC `shouldBe` 0

    it "totals the value of a node with child references" $ do
      totalValue nodeA `shouldBe` 66
