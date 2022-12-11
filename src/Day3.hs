module Day3 (part1, part2, input, sample) where

import Data.Char as C
import Data.List.Split qualified as X
import Data.Text qualified as T
import Lib
import RIO
import RIO.List
import RIO.List.Partial

day :: Integer
day = 3

input :: String
input = T.unpack $ fetch day

sample :: String
sample = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"

-- Subtract the offset from the character's ordinal to get the priority.
toPriority :: Char -> Int
toPriority c = C.ord c - (if c >= 'a' then 96 else 38)

-- Takes a function that manipulates priorities into an answer and the input,
-- then handles parsing -> conversion -> calling manipulator -> sum.
sumPriorities :: ([[Int]] -> [Int]) -> String -> Int
sumPriorities f = sum . f . map (map toPriority) . lines

-- Split each list of priorities in half, intersect each half, take only the
-- first values (which will be the double-packed item), then add them together.
part1 :: String -> Int
part1 =
  sumPriorities $
    map (\x -> head $ uncurry intersect $ splitAt (length x `div` 2) x)

-- Split the lists of priorities into three chunks, intersect all chunks, take
-- the first values (which will be the badge), then add them together.
part2 :: String -> Int
part2 =
  sumPriorities $
    map (head . foldl1' intersect) . X.chunksOf 3
