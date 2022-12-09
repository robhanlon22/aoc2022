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

type Result = Int

day :: Integer
day = 9

input :: T.Text
input = fetch day

sample :: T.Text
sample = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n"

sample2 :: T.Text
sample2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20\n"

data Dir = U | D | L | R deriving (Eq, Show)

data Move = Move {dir :: Dir, n :: Integer} deriving (Eq, Show)

newtype Input = Input {moves :: [Move]} deriving (Eq, Show)

pMove :: Parser Move
pMove = do
  dir <- choice [U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R']
  void $ char ' '
  n <- L.decimal
  return Move {..}

pInput :: Parser Input
pInput = do
  moves <- pMove `endBy` newline
  return Input {..}

data World = World
  { hPos :: (Integer, Integer),
    tPoses :: S.Seq (Integer, Integer),
    tVisited :: HS.HashSet (Integer, Integer)
  }
  deriving (Eq, Show)

newWorld :: Int -> World
newWorld nTails =
  World
    { hPos = (0, 0),
      tPoses = S.replicate nTails (0, 0),
      tVisited = HS.singleton (0, 0)
    }

deltas :: [(Integer, Integer)]
deltas = [(dx, dy) | dx <- [(-1) .. 1], dy <- [(-1) .. 1]]

isNeighbor :: (Integer, Integer) -> (Integer, Integer) -> Bool
isNeighbor (pX, pY) (tX, tY) = any (\(dx, dy) -> pX + dx == tX && pY + dy == tY) deltas

offset :: (Ord a, Num a) => a -> a -> a
offset a b = clamp (-1, 1) (a - b)

nextTail ::
  ((Integer, Integer), S.Seq (Integer, Integer)) ->
  (Integer, Integer) ->
  ((Integer, Integer), S.Seq (Integer, Integer))
nextTail (p@(pX, pY), s) t@(tX, tY) = (pos, s S.|> pos)
  where
    (dx, dy) =
      if isNeighbor p t
        then (0, 0)
        else (offset pX tX, offset pY tY)
    pos = (tX + dx, tY + dy)

move :: World -> [Move] -> World
move world [] = world
move World {hPos = (hX, hY), ..} (m@Move {..} : xs) =
  let hPos' = case dir of
        U -> (hX, hY + 1)
        D -> (hX, hY - 1)
        L -> (hX - 1, hY)
        R -> (hX + 1, hY)
      (tPos, tPoses') = foldl nextTail (hPos', S.empty) tPoses
      tVisited' = HS.insert tPos tVisited
      moves' = if n == 1 then xs else (m {n = n - 1}) : xs
      world' = World {hPos = hPos', tPoses = tPoses', tVisited = tVisited'}
   in move world' moves'

go :: [Move] -> Int -> Int
go moves n = let World {..} = move (newWorld n) moves in HS.size tVisited

part1 :: Input -> Result
part1 Input {..} = go moves 1

part2 :: Input -> Result
part2 Input {..} = go moves 9

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
