{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2, input, sample) where

import Control.Monad (liftM2)
import Data.Text (Text)
import Lib (Parser, doParse, fetch)
import Text.Megaparsec (choice, empty, many)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 empty empty

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round Move Move

data Result = Result Move Ordering

instance Ord Move where
  Rock `compare` Rock = EQ
  Rock `compare` Paper = LT
  Rock `compare` Scissors = GT
  Paper `compare` Rock = GT
  Paper `compare` Paper = EQ
  Paper `compare` Scissors = LT
  Scissors `compare` Rock = LT
  Scissors `compare` Paper = GT
  Scissors `compare` Scissors = EQ

day :: Integer
day = 2

sample :: Text
sample = "A Y\nB X\nC Z\n"

input :: Text
input = fetch day

pMove :: Parser Move
pMove =
  choice
    [ Rock <$ char 'A',
      Paper <$ char 'B',
      Scissors <$ char 'C',
      Rock <$ char 'X',
      Paper <$ char 'Y',
      Scissors <$ char 'Z'
    ]

pOrdering :: Parser Ordering
pOrdering =
  choice
    [ GT <$ char 'X',
      EQ <$ char 'Y',
      LT <$ char 'Z'
    ]

pRound :: Parser Round
pRound = do
  opponent <- lexeme pMove
  self <- lexeme pMove
  return $ Round opponent self

pResult :: Parser Result
pResult = do
  opponent <- lexeme pMove
  ordering <- lexeme pOrdering
  return $ Result opponent ordering

orderingScore :: Ordering -> Integer
orderingScore LT = 6
orderingScore EQ = 3
orderingScore GT = 0

outcome :: Round -> Ordering
outcome (Round opponent self) = opponent `compare` self

outcomeScore :: Round -> Integer
outcomeScore = orderingScore . outcome

moveScore :: Round -> Integer
moveScore (Round _ Rock) = 1
moveScore (Round _ Paper) = 2
moveScore (Round _ Scissors) = 3

roundScore :: Round -> Integer
roundScore = liftM2 (+) moveScore outcomeScore

toRound :: Result -> Round
toRound result@(Result opponent _) =
  let move = case result of
        Result Rock GT -> Scissors
        Result Rock EQ -> Rock
        Result Rock LT -> Paper
        Result Paper GT -> Rock
        Result Paper EQ -> Paper
        Result Paper LT -> Scissors
        Result Scissors GT -> Paper
        Result Scissors EQ -> Scissors
        Result Scissors LT -> Rock
   in Round opponent move

resultScore :: Result -> Integer
resultScore = roundScore . toRound

pPart1 :: Parser [Round]
pPart1 = many pRound

pPart2 :: Parser [Result]
pPart2 = many pResult

part1 :: Text -> Integer
part1 = sum . map roundScore . doParse pPart1

part2 :: Text -> Integer
part2 = sum . map resultScore . doParse pPart2
