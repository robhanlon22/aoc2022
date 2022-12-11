{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day8 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Lib (Parser, countBy, fetchSafe, solve)
import RIO
import RIO.List.Partial
import RIO.Partial
import Text.Megaparsec (MonadParsec (lookAhead), endBy, manyTill)
import Text.Megaparsec.Char (digitChar, newline)

type World = V.Vector (V.Vector Integer)

type Result = Int

day :: Integer
day = 8

input :: IO T.Text
input = fetchSafe day

sample :: IO T.Text
sample = return "30373\n25512\n65332\n33549\n35390\n"

pDigit :: Parser Integer
pDigit = do
  digit <- digitChar
  return (read [digit] :: Integer)

pDigits :: Parser (V.Vector Integer)
pDigits = V.fromList <$> manyTill pDigit (lookAhead newline)

pWorld :: Parser World
pWorld = V.fromList <$> (pDigits `endBy` newline)

data Surroundings = Surroundings
  { cell :: Integer,
    rays :: [V.Vector Integer]
  }
  deriving (Eq, Show)

surroundings :: V.Vector (V.Vector Integer) -> [Surroundings]
surroundings i = concatMap (map surrounds) indices
  where
    height = V.length i
    width = V.length $ i V.! 0
    rows = [0 .. (pred height)]
    cols = [0 .. (pred width)]
    indices = map (\row -> map (row,) cols) rows
    getValue (row, col) = i V.! row V.! col
    surrounds coord@(row, col) =
      let cell = getValue coord
          rays =
            map
              (V.fromList . map getValue)
              [ [(row, c) | c <- [(pred col), (pred (pred col)) .. 0]],
                [(row, c) | c <- [(succ col) .. (pred width)]],
                [(r, col) | r <- [(succ row) .. (pred height)]],
                [(r, col) | r <- [(pred row), (pred (pred row)) .. 0]]
              ]
       in Surroundings {..}

part1 :: World -> Result
part1 = countBy (\Surroundings {..} -> any (all (cell >)) rays) . surroundings

part2 :: World -> Result
part2 =
  maximum
    . map
      ( \Surroundings {..} ->
          product
            ( map
                ( \ray ->
                    min
                      (V.length (V.takeWhile (cell >) ray) + 1)
                      (V.length ray)
                )
                rays
            )
      )
    . surroundings

solve' :: (World -> Result) -> T.Text -> Result
solve' part = solve part pWorld

part1Sample :: IO Result
part1Sample = solve' part1 <$> sample

part1Input :: IO Result
part1Input = solve' part1 <$> input

part2Sample :: IO Result
part2Sample = solve' part2 <$> sample

part2Input :: IO Result
part2Input = solve' part2 <$> input
