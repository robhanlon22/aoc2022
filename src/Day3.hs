module Day3 (part1, part2, input, sample) where

import Control.Monad (liftM2)
import Data.Char (ord)
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)
import Data.Text (unpack)
import Lib (fetch)

day :: Integer
day = 3

input :: String
input = unpack $ fetch day

sample :: String
sample = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n"

offset :: Num a => Char -> a
offset char
  | char >= 'a' = 96
  | otherwise = 38

toOrds :: [[Char]] -> [[Int]]
toOrds = map $ map $ liftM2 (-) ord offset

part1 :: String -> Int
part1 =
  sum
    . map (head . nub . uncurry intersect . (splitAt =<< (`div` 2) . length))
    . toOrds
    . lines

part2 :: String -> Int
part2 =
  sum
    . map (head . nub . foldl1 intersect)
    . chunksOf 3
    . toOrds
    . lines
