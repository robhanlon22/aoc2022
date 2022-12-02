{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2, input, sample) where

import Control.Monad (void)
import Data.Text (Text)
import Lib (Parser, doParse, fetch)
import Text.Megaparsec (choice, endBy)
import Text.Megaparsec.Char (char, newline)

data Move = Rock | Paper | Scissors deriving (Eq, Show)

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

pOutcome :: Parser Ordering
pOutcome =
  choice
    [ GT <$ char 'X',
      EQ <$ char 'Y',
      LT <$ char 'Z'
    ]

pRound :: Parser (Move, Move)
pRound = do
  opponent <- pMove
  void $ char ' '
  self <- pMove
  return (opponent, self)

pFate :: Parser (Move, Ordering)
pFate = do
  opponent <- pMove
  void $ char ' '
  outcome <- pOutcome
  return (opponent, outcome)

outcomeScore :: (Move, Move) -> Integer
outcomeScore (opponent, self) = f (opponent `compare` self)
  where
    f LT = 6
    f EQ = 3
    f GT = 0

moveScore :: Move -> Integer
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

roundScore :: (Move, Move) -> Integer
roundScore r@(_, self) = moveScore self + outcomeScore r

toRound :: (Move, Ordering) -> (Move, Move)
toRound (Rock, GT) = (Rock, Scissors)
toRound (Rock, EQ) = (Rock, Rock)
toRound (Rock, LT) = (Rock, Paper)
toRound (Paper, GT) = (Paper, Rock)
toRound (Paper, EQ) = (Paper, Paper)
toRound (Paper, LT) = (Paper, Scissors)
toRound (Scissors, GT) = (Scissors, Paper)
toRound (Scissors, EQ) = (Scissors, Scissors)
toRound (Scissors, LT) = (Scissors, Rock)

fateScore :: (Move, Ordering) -> Integer
fateScore = roundScore . toRound

pPart1 :: Parser [(Move, Move)]
pPart1 = pRound `endBy` newline

pPart2 :: Parser [(Move, Ordering)]
pPart2 = pFate `endBy` newline

part1 :: Text -> Integer
part1 = sum . map roundScore . doParse pPart1

part2 :: Text -> Integer
part2 = sum . map fateScore . doParse pPart2
