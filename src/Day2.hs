{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2, input, sample) where

import Data.Text (Text)
import Lib (Parser, doParse, fetch)
import Text.Megaparsec (choice, empty, many)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (round)

day :: Integer
day = 2

input :: Text
input = fetch day

sample :: Text
sample = "A Y\nB X\nC Z\n"

data Move = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round Move Move

data Outcome = Lose | Draw | Win deriving (Eq, Show)

data Result = Result Move Outcome

class Scorable a where
  score :: a -> Integer

instance Scorable Move where
  score Rock = 1
  score Paper = 2
  score Scissors = 3

instance Scorable Outcome where
  score Lose = 0
  score Draw = 3
  score Win = 6

instance Scorable Round where
  score (Round opponent self) =
    score self
      + score
        ( case self of
            Rock -> case opponent of
              Rock -> Draw
              Paper -> Lose
              Scissors -> Win
            Paper -> case opponent of
              Rock -> Win
              Paper -> Draw
              Scissors -> Lose
            Scissors -> case opponent of
              Rock -> Lose
              Paper -> Win
              Scissors -> Draw
        )

instance Scorable Result where
  score (Result opponent outcome) =
    score $
      Round
        opponent
        ( case outcome of
            Win -> case opponent of
              Rock -> Paper
              Paper -> Scissors
              Scissors -> Rock
            Lose -> case opponent of
              Rock -> Scissors
              Paper -> Rock
              Scissors -> Paper
            Draw -> case opponent of
              Rock -> Rock
              Paper -> Paper
              Scissors -> Scissors
        )

instance Scorable a => Scorable [a] where
  score = sum . map score

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 empty empty

pMove :: Parser Move
pMove =
  lexeme $
    choice
      [ Rock <$ char 'A',
        Paper <$ char 'B',
        Scissors <$ char 'C',
        Rock <$ char 'X',
        Paper <$ char 'Y',
        Scissors <$ char 'Z'
      ]

pOutcome :: Parser Outcome
pOutcome =
  lexeme $
    choice
      [ Lose <$ char 'X',
        Draw <$ char 'Y',
        Win <$ char 'Z'
      ]

pRound :: Parser Round
pRound = pMove >>= (<$> pMove) . Round

pResult :: Parser Result
pResult = pMove >>= (<$> pOutcome) . Result

part :: (Scorable a) => Parser a -> Text -> Integer
part = (score .) . doParse . many

part1 :: Text -> Integer
part1 = part pRound

part2 :: Text -> Integer
part2 = part pResult
