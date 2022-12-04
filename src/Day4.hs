{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day4 (part1, part2, input, sample) where

import Control.Monad (void)
import Data.List (intersect)
import Data.Text (Text)
import Lib (Parser, doParse, fetch)
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as L

data Pair = Pair {lhs, rhs :: [Int]}

data Inter = Inter {pair :: Pair, inter :: [Int]}

day :: Integer
day = 4

sample :: Text
sample = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\n"

input :: Text
input = fetch day

pRange :: Parser [Int]
pRange = do
  lhs <- L.decimal
  void $ char '-'
  rhs <- L.decimal
  return [lhs .. rhs]

pPair :: Parser Pair
pPair = do
  lhs <- pRange
  void $ char ','
  rhs <- pRange
  return $ Pair {..}

pInput :: Parser [Pair]
pInput = pPair `endBy` newline

parse :: Text -> [Pair]
parse = doParse pInput

solve :: (Inter -> Bool) -> Text -> Int
solve f =
  length
    . filter f
    . map (\pair@(Pair {..}) -> let inter = lhs `intersect` rhs in Inter {..})
    . parse

part1 :: Text -> Int
part1 = solve $ \(Inter {pair = Pair {..}, ..}) -> inter == lhs || inter == rhs

part2 :: Text -> Int
part2 = solve $ \(Inter {..}) -> not $ null inter
