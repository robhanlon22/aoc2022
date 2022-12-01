module Day1 (part1, part2) where

import Data.List (sortBy)
import Data.Text (Text)
import Lib (Parser, fetch, parse)
import Text.Megaparsec
  ( endBy,
    sepBy,
  )
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

input :: IO Text
input = fetch 1

elfCalories :: Text -> [Integer]
elfCalories = map sum . parse ((decimal `endBy` newline) `sepBy` newline)

part1' :: Text -> Integer
part1' = maximum . elfCalories

part1 :: IO Integer
part1 = part1' <$> input

part2' :: Text -> Integer
part2' = sum . take 3 . sortBy (flip compare) . elfCalories

part2 :: IO Integer
part2 = part2' <$> input
