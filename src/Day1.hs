module Day1 (part1, part2) where

import Control.Monad (void)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Void (Void)
import Lib (fetch, parse)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    many,
  )
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

input :: IO Text
input = fetch 1

pCalories :: Parser [[Integer]]
pCalories = many . try $ do
  ints <- many $ do
    int <- decimal
    void newline
    return int
  void $ try newline
  return ints

elfCalories :: Text -> [Integer]
elfCalories = map sum . parse pCalories

part1' :: Text -> Integer
part1' = maximum . elfCalories

part1 :: IO Integer
part1 = part1' <$> input

part2' :: Text -> Integer
part2' = sum . take 3 . sortBy (flip compare) . elfCalories

part2 :: IO Integer
part2 = part2' <$> input
