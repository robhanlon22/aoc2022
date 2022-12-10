{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day4 (part1, part2, input, sample) where

import Control.Monad (void)
import Data.Range (Range, union, (+=+))
import Data.Text (Text)
import Lib (Parser, countBy, doParse, fetch)
import Text.Megaparsec (endBy)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Megaparsec.Char.Lexer as L

data Pair = Pair {lhs, rhs :: Range Integer}

data Unions = Unions {pair :: Pair, unions :: [Range Integer]}

day :: Integer
day = 4

sample :: Text
sample = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\n"

input :: Text
input = fetch day

pRange :: Parser (Range Integer)
pRange = do
  lhs <- L.decimal
  void $ char '-'
  rhs <- L.decimal
  return $ lhs +=+ rhs

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

-- Given a Pair, make a Unions, which contains the Pair and the unions of the
-- Ranges in the pair.
newUnions :: Pair -> Unions
newUnions pair@(Pair {..}) = let unions = [lhs] `union` [rhs] in Unions {..}

containing :: (Pair -> Range Integer -> Bool) -> Unions -> Bool
containing f (Unions {..}) = check unions
  where
    check [] = False
    check [i] = f pair i
    check _ = False

-- Solves the puzzle! f is a function that will be used to filter a list of
-- Unions. The solution is the number of Unions that pass the test.
solve :: (Pair -> Range Integer -> Bool) -> Text -> Int
solve f = countBy (containing f) . map newUnions . parse

-- A Unions is considered encompassing if its unions list is a continuous and its
-- only element is equal to either Range it its Pair.
part1 :: Text -> Int
part1 = solve (\Pair {..} i -> i == lhs || i == rhs)

-- A Unions is considered overlapping if its unions list is a continuous.
part2 :: Text -> Int
part2 = solve (\_ _ -> True)
