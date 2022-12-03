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

-- Compute the offset to change a character's ordinal to its priority.
offset :: Num a => Char -> a
offset char
  | char >= 'a' = 96
  | otherwise = 38

-- Subtract the offset from the character's priority.
toPriority :: Char -> Int
toPriority c = ord c + offset c

-- Turn all nested chars into their priorities.
toPriorities :: [[Char]] -> [[Int]]
toPriorities = map $ map toPriority

-- Takes a function that manipulates priorities into an answer and the input,
-- then handles parsing -> conversion -> calling manipulator -> sum.
sumPriorities :: ([[Int]] -> [Int]) -> String -> Int
sumPriorities f = sum . f . toPriorities . lines

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
