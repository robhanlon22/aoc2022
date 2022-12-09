{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day9 (part1Sample, part1Input, part2Sample, part2Sample2, part2Input, input, sample) where

import Control.Monad (void)
import Data.HashSet qualified as HS
import Data.Ord (clamp)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Lib (Parser, fetch, solve)
import Text.Megaparsec (choice, endBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer qualified as L

type Result = Either Error Int

day :: Integer
day = 9

input :: T.Text
input = fetch day

sample :: T.Text
sample = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n"

sample2 :: T.Text
sample2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20\n"

data Dir = U | D | L | R deriving (Eq, Show)

type Input = [Dir]

data Error = NoTailsError deriving (Eq, Show)

type Point = (Integer, Integer)

data World = World
  { hPos :: Point,
    tPoses :: S.Seq Point,
    tVisited :: HS.HashSet Point
  }
  deriving (Eq, Show)

pMove :: Parser [Dir]
pMove = do
  dir <-
    choice
      [ U <$ char 'U',
        D <$ char 'D',
        L <$ char 'L',
        R <$ char 'R'
      ]
  void $ char ' '
  n <- L.decimal
  return $ replicate n dir

pInput :: Parser Input
pInput = concat <$> (pMove `endBy` newline)

origin :: Point
origin = (0, 0)

newWorld :: Int -> Either Error World
newWorld nTails
  | nTails <= 0 = Left NoTailsError
  | otherwise =
      Right
        World
          { hPos = origin,
            tPoses = S.replicate nTails origin,
            tVisited = HS.singleton origin
          }

deltas :: [Point]
deltas = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

isNeighbor :: Point -> Point -> Bool
isNeighbor p t = any (\d -> p +:+ d == t) deltas

tailDelta :: (Ord a, Num a) => a -> a -> a
tailDelta a b = clamp (-1, 1) (a - b)

adjustTail :: (Ord a, Ord b, Num a, Num b) => (a, b) -> (a, b) -> (a, b)
adjustTail (pX, pY) t@(tX, tY) =
  t +:+ (tailDelta pX tX, tailDelta pY tY)

nextTail :: (Point, S.Seq Point) -> Point -> (Point, S.Seq Point)
nextTail (p, s) t = (t', s S.|> t')
  where
    t' = if p `isNeighbor` t then t else adjustTail p t

dirDeltas :: (Num a, Num b) => Dir -> (a, b)
dirDeltas U = (0, 1)
dirDeltas D = (0, -1)
dirDeltas L = (-1, 0)
dirDeltas R = (1, 0)

(+:+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+:+) (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

move :: World -> [Dir] -> World
move world [] = world
move World {..} (dir : dirs) = move world dirs
  where
    hPos' = hPos +:+ dirDeltas dir
    (tPos, tPoses') = foldl nextTail (hPos', S.empty) tPoses
    tVisited' = HS.insert tPos tVisited
    world =
      World
        { hPos = hPos',
          tPoses = tPoses',
          tVisited = tVisited'
        }

go :: Int -> Input -> Either Error Int
go n moves = do
  world <- newWorld n
  return $ HS.size $ tVisited $ move world moves

part1 :: Input -> Result
part1 = go 1

part2 :: Input -> Result
part2 = go 9

solve' :: (Input -> Result) -> T.Text -> Result
solve' part = solve part pInput

part1Sample :: Result
part1Sample = solve' part1 sample

part1Input :: Result
part1Input = solve' part1 input

part2Sample :: Result
part2Sample = solve' part2 sample

part2Sample2 :: Result
part2Sample2 = solve' part2 sample2

part2Input :: Result
part2Input = solve' part2 input
