module Day3 (part1, part2, input, sample) where

import Data.Char (ord)
import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Text (unpack)
import Lib (fetch)

day :: Integer
day = 3

input :: String
input = unpack $ fetch day

sample :: String
sample = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"

-- Subtract the offset from the character's priority.
toPriority :: Char -> Int
toPriority c = ord c - (if c >= 'a' then 96 else 38)

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
    map (head . foldl1 intersect) . chunksOf 3
