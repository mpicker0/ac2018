module AC.Dec16 (
  part1
  , parseSamples
  , parseSample
  , parseRawInstruction
  , runInstruction
  , sampleWorksForOpcode
  , Sample (..)
  , Opcode (..)
  , Instruction (..)
  , RawInstruction
  , part2
  ) where

import System.Environment
import Helpers
import Control.Arrow
import Data.List
import Data.Bits
import Text.Regex.Posix hiding (before, after)

main = do
    [problem, filename1, filename2] <- getArgs
    sampleInput <- stringsFromFile filename1
    let samples = parseSamples sampleInput
    instructions <- thingsFromFile filename2 parseRawInstruction
    putStrLn (case problem of
               "1" -> show (part1 samples)
               "2" -> show (part2 instructions))

type RawInstruction = [Int]
type Registers = [Int]

-- It would be nice if I could require the arrays to have length 4.  I could
-- create another type but that seems heavy.
data Sample = Sample { before :: Registers
                     , instruction :: RawInstruction
                     , after :: Registers
                     } deriving (Eq, Show)

data Opcode = Addr | Addi
            | Mulr | Muli
            | Banr | Bani
            | Borr | Bori
            | Setr | Seti
            | Gtir | Gtri | Gtrr
            | Eqir | Eqri | Eqrr
            deriving (Ord, Show, Eq, Bounded, Enum)

data Instruction =
  Instruction { opcode :: Opcode
              , a :: Int
              , b :: Int
              , c :: Int
              }

part1 :: [Sample] -> Int
part1 samples =
  let allOpcodes = [minBound .. maxBound] :: [Opcode]
      countMatches sample = map (sampleWorksForOpcode sample) >>>
        occurrences (==True) $ allOpcodes
      matchArray = map countMatches samples
  in occurrences (>= 3) matchArray

-- Determine if a given sample holds true for a particular opcode; that is, if
-- the after state of the sample is consistent with executing the opcode against
-- the before state.  Arguments:  sample, opcode
sampleWorksForOpcode :: Sample -> Opcode -> Bool
sampleWorksForOpcode (Sample before (_:a:b:c:_) after) opcode =
  let instruction = Instruction opcode a b c
      newRegisters = runInstruction instruction before
  in newRegisters == after

-- Take a list of 'a' and break it into several lists of 'a' of a given size
-- Arguments:  chunk size, list
-- Inspired by https://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (heads, tails) = splitAt n xs
  in heads:chunksOf n tails

-- Take a list of String and produce a list of Sample.  It takes multiple lines
-- of input to complete a Sample, so we chunk the input into 4 lines (3 lines
-- of actual data, and a blank line), then map that onto parseSample.
parseSamples :: [String] -> [Sample]
parseSamples s =
  let rawSamples = chunksOf 4 s
  in map (\rs -> parseSample rs) rawSamples

-- Parse a list of String into a sample.  The input file has three strings per
-- sample.
parseSample :: [String] -> Sample
parseSample (b:instruction:a:_) =
  let [[_, before]] = b =~ "^Before: \\[([0-9, ]*)\\]$" :: [[String]]
      [[_, after]] = a =~ "^After:  \\[([0-9, ]*)\\]$" :: [[String]]
      numbers = filter (/=',') >>> words >>> map(\w -> read w :: Int)
  in Sample { before = numbers before
            , instruction = numbers instruction
            , after = numbers after
            }

-- Run an instructions and return the new state of the registers.  Arguments:
-- Instruction to run, current state of registers
runInstruction :: Instruction -> Registers -> Registers
runInstruction (Instruction opcode a b c) registers =
  case opcode of
    Addr -> setc (ra + rb) registers
    Addi -> setc (ra + b) registers
    Mulr -> setc (ra * rb) registers
    Muli -> setc (ra * b) registers
    Banr -> setc (ra .&. rb) registers
    Bani -> setc (ra .&. b) registers
    Borr -> setc (ra .|. rb) registers
    Bori -> setc (ra .|. b) registers
    Setr -> setc ra registers
    Seti -> setc a registers
    Gtir -> setc (if a > rb then 1 else 0) registers
    Gtri -> setc (if ra > b then 1 else 0) registers
    Gtrr -> setc (if ra > rb then 1 else 0) registers
    Eqir -> setc (if a == rb then 1 else 0) registers
    Eqri -> setc (if ra == b then 1 else 0) registers
    Eqrr -> setc (if ra == rb then 1 else 0) registers
  where ra = registers!!a
        rb = registers!!b
        rc = registers!!c
        setc val regs =
          let (h, _:t) = splitAt c regs
          in h ++ [val] ++ t

parseRawInstruction :: String -> RawInstruction
parseRawInstruction = words >>> map(\w -> read w :: Int)

part2 :: [RawInstruction] -> Int
part2 instructions = -2
