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

data Move = Rock | Paper | Scissors deriving (Eq, Show, Enum, Bounded)

data Round = Round Move Move

data Result = Result Move Ordering

class Scorable a where
  score :: a -> Integer

instance Scorable Move where
  score Rock = 1
  score Paper = 2
  score Scissors = 3

instance Scorable Ordering where
  score LT = 6
  score EQ = 3
  score GT = 0

instance Scorable Round where
  score round@(Round _ self) = score self + score (outcome round)

instance Scorable Result where
  score (Result opponent ordering) =
    score (Round opponent self)
    where
      causesOutcome m = outcome (Round opponent m) == ordering
      predMove = cyclicPred opponent
      succMove = cyclicSucc opponent
      self
        | causesOutcome predMove = predMove
        | causesOutcome succMove = succMove
        | otherwise = opponent

instance Scorable a => Scorable [a] where
  score = sum . map score

cyclicSucc :: (Enum a, Bounded a, Eq a) => a -> a
cyclicSucc x
  | x == maxBound = minBound
  | otherwise = succ x

cyclicPred :: (Enum a, Bounded a, Eq a) => a -> a
cyclicPred x
  | x == minBound = maxBound
  | otherwise = pred x

instance Ord Move where
  compare x y
    | x == cyclicPred y = LT
    | x == cyclicSucc y = GT
    | otherwise = EQ

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

pOrdering :: Parser Ordering
pOrdering =
  lexeme $
    choice
      [ GT <$ char 'X',
        EQ <$ char 'Y',
        LT <$ char 'Z'
      ]

pRound :: Parser Round
pRound = [Round opponent self | opponent <- pMove, self <- pMove]

pResult :: Parser Result
pResult = [Result opponent ordering | opponent <- pMove, ordering <- pOrdering]

outcome :: Round -> Ordering
outcome (Round opponent self) = opponent `compare` self

part :: (Scorable a) => Parser a -> Text -> Integer
part p t = score $ doParse (many p) t

part1 :: Text -> Integer
part1 = part pRound

part2 :: Text -> Integer
part2 = part pResult
