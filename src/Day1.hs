module Day1 (part1, part2) where

import Data.List (sort)
import Data.List.Split (splitWhen)
import Lib (fetchInput)

input :: IO String
input = fetchInput 1

elfCalories :: String -> [Integer]
elfCalories =
  map (sum . map (read :: String -> Integer)) . splitWhen (== "") . lines

part1' :: String -> Integer
part1' = maximum . elfCalories

part1 :: IO ()
part1 = do
  str <- input
  print (part1' str)

part2' :: String -> Integer
part2' =
  sum . take 3 . reverse . sort . elfCalories

part2 :: IO ()
part2 = do
  str <- input
  print (part2' str)
