{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day8 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Lib (Parser, fetch, solve)
import Text.Megaparsec (MonadParsec (lookAhead), endBy, manyTill)
import Text.Megaparsec.Char (digitChar, newline)

type World = V.Vector (V.Vector Integer)

type Result = Int

day :: Integer
day = 8

input :: T.Text
input = fetch day

sample :: T.Text
sample = "30373\n25512\n65332\n33549\n35390\n"

pDigit :: Parser Integer
pDigit = do
  digit <- digitChar
  return (read [digit] :: Integer)

pDigits :: Parser (V.Vector Integer)
pDigits = V.fromList <$> manyTill pDigit (lookAhead newline)

pWorld :: Parser World
pWorld = V.fromList <$> (pDigits `endBy` newline)

data Cell = Cell {value :: Integer, coord :: (Int, Int)} deriving (Eq, Show)

instance Ord Cell where
  compare Cell {value = a} Cell {value = b} = a `compare` b

data Surroundings = Surroundings
  { cell :: Cell,
    rays :: [[Cell]]
  }
  deriving (Eq, Show)

surroundings :: V.Vector (V.Vector Integer) -> [Surroundings]
surroundings i =
  let height = V.length i
      width = V.length $ i V.! 0
      indices =
        map
          (\row -> map (row,) [0 .. (pred width)])
          [0 .. (pred height)]
      makeCell coord@(row, col) = Cell {value = i V.! row V.! col, ..}
   in concatMap
        ( map
            ( \coord@(row, col) ->
                let left =
                      [ makeCell (row, c)
                        | c <- [(pred col), (pred (pred col)) .. 0]
                      ]
                    right =
                      [ makeCell (row, c)
                        | c <- [(succ col) .. (pred width)]
                      ]
                    up =
                      [ makeCell (r, col)
                        | r <- [(pred row), (pred (pred row)) .. 0]
                      ]
                    down =
                      [ makeCell (r, col)
                        | r <- [(succ row) .. (pred height)]
                      ]
                 in Surroundings
                      { cell = makeCell coord,
                        rays = filter (not . null) [left, right, up, down]
                      }
            )
        )
        indices

part1 :: World -> Result
part1 =
  sum
    . map
      (\Surroundings {..} -> if any (all (cell >)) rays then 1 else 0)
    . surroundings

part2 :: World -> Result
part2 =
  maximum
    . map
      ( \Surroundings {..} ->
          product
            ( map
                ( \ray ->
                    min
                      (length (takeWhile (cell >) ray) + 1)
                      (length ray)
                )
                rays
            )
      )
    . surroundings

solve' :: (World -> Result) -> T.Text -> Result
solve' part = solve part pWorld

part1Sample :: Result
part1Sample = solve' part1 sample

part1Input :: Result
part1Input = solve' part1 input

part2Sample :: Result
part2Sample = solve' part2 sample

part2Input :: Result
part2Input = solve' part2 input
