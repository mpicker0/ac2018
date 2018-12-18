module AC.Dec16Spec (spec) where

import Test.Hspec hiding (before, after)
import AC.Dec16
import Helpers

spec :: Spec
spec = do
  let exampleSample = Sample { before = [3, 2, 1, 1]
                      , instruction = [9, 2, 1, 2]
                      , after = [3, 2, 2, 1]
                      }

  describe "part1" $ do
    it "solves the example" $ do
      part1 [exampleSample] `shouldBe` 1

  describe "parseSamples" $ do
    it "parses a Sample from a collection of Strings" $ do
      let input = [ "Before: [3, 2, 1, 1]"
                  , "9 2 1 2"
                  , "After:  [3, 2, 2, 1]"
                  ]
      parseSamples input `shouldBe` [exampleSample]

  describe "parseSample" $ do
    it "parses a Sample from a collection of Strings" $ do
      let input = [ "Before: [3, 2, 1, 1]"
                  , "9 2 1 2"
                  , "After:  [3, 2, 2, 1]"
                  ]
      parseSample input `shouldBe` exampleSample

  describe "parseRawInstruction" $ do
    it "parses a raw instruction (one without an opcode)" $ do
      parseRawInstruction "1 2 3 4" `shouldBe` [1, 2, 3, 4]

  describe "sampleWorksForOpcode" $ do
    it "returns True when a sample works for an opcode" $ do
      sampleWorksForOpcode exampleSample Mulr `shouldBe` True

    it "returns False when a sample does not work for an opcode" $ do
      sampleWorksForOpcode exampleSample Eqrr `shouldBe` False

  describe "runInstruction" $ do
    describe "included examples" $ do
      let state1 = before exampleSample
      let state2 = after exampleSample

      it "runs the example for Mulr" $ do
        runInstruction (Instruction Mulr 2 1 2) state1 `shouldBe` state2

      it "runs the example for Addi" $ do
        runInstruction (Instruction Addi 2 1 2) state1 `shouldBe` state2

      it "runs the example for Seti" $ do
        runInstruction (Instruction Seti 2 1 2) state1 `shouldBe` state2

    describe "addition" $ do
      it "runs Addr" $ do
        let state1 = [0, 1, 2, 0]
            state2 = [0, 1, 2, 3]
        runInstruction (Instruction Addr 1 2 3) state1 `shouldBe` state2

    describe "multiplication" $ do
      it "runs Muli" $ do
        let state1 = [0, 1, 2, 0]
            state2 = [0, 1, 2, 2]
        runInstruction (Instruction Muli 1 2 3) state1 `shouldBe` state2

    describe "bitwise and" $ do
      it "runs Banr" $ do
        let state1 = [14, 11, 2, 0]
            state2 = [14, 11, 10, 0]
        runInstruction (Instruction Banr 0 1 2) state1 `shouldBe` state2

      it "runs Bani" $ do
        let state1 = [14, 0, 2, 0]
            state2 = [14, 0, 10, 0]
        runInstruction (Instruction Bani 0 11 2) state1 `shouldBe` state2

    describe "bitwise or" $ do
      it "runs Borr" $ do
        let state1 = [12, 10, 2, 0]
            state2 = [12, 10, 14, 0]
        runInstruction (Instruction Borr 0 1 2) state1 `shouldBe` state2

      it "runs Bori" $ do
        let state1 = [12, 0, 2, 0]
            state2 = [12, 0, 14, 0]
        runInstruction (Instruction Bori 0 10 2) state1 `shouldBe` state2

    describe "assignment" $ do
      it "runs Setr" $ do
        let state1 = [1, 0, 0, 0]
            state2 = [1, 0, 0, 1]
        runInstruction (Instruction Setr 0 99 3) state1 `shouldBe` state2

    describe "greater-than testing" $ do
      describe "Gtir" $ do
        it "handles greater than" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 1]
          runInstruction (Instruction Gtir 99 0 3) state1 `shouldBe` state2

        it "handles not greater than" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 0]
          runInstruction (Instruction Gtir 1 0 3) state1 `shouldBe` state2

      describe "Gtri" $ do
        it "handles greater than" $ do
          let state1 = [0, 99, 0, 0]
              state2 = [0, 99, 0, 1]
          runInstruction (Instruction Gtri 1 0 3) state1 `shouldBe` state2

        it "handles not greater than" $ do
          let state1 = [0, 1, 0, 0]
              state2 = [0, 1, 0, 0]
          runInstruction (Instruction Gtri 1 99 3) state1 `shouldBe` state2

      describe "Gtrr" $ do
        it "handles greater than" $ do
          let state1 = [2, 1, 0, 0]
              state2 = [2, 1, 0, 1]
          runInstruction (Instruction Gtrr 0 1 3) state1 `shouldBe` state2

        it "handles not greater than" $ do
          let state1 = [1, 2, 0, 0]
              state2 = [1, 2, 0, 0]
          runInstruction (Instruction Gtrr 0 1 3) state1 `shouldBe` state2

    describe "equality testing" $ do
      describe "Eqir" $ do
        it "handles equal" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 1]
          runInstruction (Instruction Eqir 1 0 3) state1 `shouldBe` state2

        it "handles not equal" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 0]
          runInstruction (Instruction Eqir 2 0 3) state1 `shouldBe` state2

      describe "Eqri" $ do
        it "handles equal" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 1]
          runInstruction (Instruction Eqri 1 0 3) state1 `shouldBe` state2

        it "handles not equal" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 0]
          runInstruction (Instruction Eqri 0 0 3) state1 `shouldBe` state2

      describe "Eqrr" $ do
        it "handles equal" $ do
          let state1 = [1, 1, 0, 0]
              state2 = [1, 1, 0, 1]
          runInstruction (Instruction Eqrr 0 1 3) state1 `shouldBe` state2

        it "handles not equal" $ do
          let state1 = [1, 0, 0, 0]
              state2 = [1, 0, 0, 0]
          runInstruction (Instruction Eqrr 0 1 3) state1 `shouldBe` state2
