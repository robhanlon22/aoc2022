{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Control.Monad (void)
import Data.List.Split (chunksOf)
import Data.Text qualified as T
import Lib (Parser, fetch, solve)
import Text.Megaparsec (choice, endBy)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Input = [Maybe Int]

day :: Integer
day = 10

input :: T.Text
input = fetch day

sample :: T.Text
sample = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n"

signedInt :: Parser Int
signedInt =
  signed (return ()) decimal

pInstruction :: Parser (Maybe Int)
pInstruction =
  choice
    [ Nothing <$ string "noop",
      Just <$> do
        void $ string "addx "
        signedInt
    ]

pInput :: Parser Input
pInput = pInstruction `endBy` newline

execute :: [Maybe Int] -> [Int]
execute i =
  reverse $
    snd $
      foldl
        ( \(x, h) ins ->
            let h' = x : h
             in maybe
                  (x, h')
                  (\y -> (x + y, x : h'))
                  ins
        )
        (1, [])
        i

part1 :: Input -> Int
part1 i =
  let h = execute i
   in sum $
        map
          (\n -> (h !! pred n) * n)
          [20, 60, 100, 140, 180, 220]

screenWidth :: Int
screenWidth = 40

part2 :: Input -> T.Text
part2 i =
  T.pack $
    unlines $
      chunksOf screenWidth $
        zipWith
          ( \x n ->
              if n >= pred x && n <= succ x
                then '#'
                else '.'
          )
          (execute i)
          (cycle [0 .. pred screenWidth])

solve' :: (Input -> b) -> T.Text -> b
solve' part = solve part pInput

part1Sample :: Int
part1Sample = solve' part1 sample

part1Input :: Int
part1Input = solve' part1 input

part2Sample :: T.Text
part2Sample = solve' part2 sample

part2Input :: T.Text
part2Input = solve' part2 input
