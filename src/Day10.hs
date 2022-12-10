{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Control.Monad (void)
import Data.Foldable
-- import qualified Data.HashSet as HS
-- import qualified Data.Vector as V

import Data.List.Split (chunksOf)
-- import qualified Data.HashMap.Lazy as HM

import Data.Maybe (catMaybes, mapMaybe)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Debug.Trace
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Pretty.Simple

-- import Data.List.Split

type Input = [Maybe Integer]

type Result = Integer

day :: Integer
day = 10

input :: T.Text
input = fetch day

sample :: T.Text
sample = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n"

signedInteger :: Parser Integer
signedInteger =
  L.signed (return ()) L.decimal

pInstruction :: Parser (Maybe Integer)
pInstruction =
  choice
    [ Nothing <$ string "noop",
      Just <$> do
        void $ string "addx "
        signedInteger
    ]

pInput :: Parser Input
pInput = pInstruction `endBy` newline

data World = World {x, c :: Integer} deriving (Eq, Show)

execute i = exec i (World {x = 1, c = 1}, S.singleton (World {x = 1, c = 0}))
  where
    exec [] (w, h) = toList $ h S.|> w
    exec (Just y : xs) (w, h) =
      let w0 = w
          h0 = h
          (w1, h1) = (w0 {c = succ (c w0)}, h0 S.|> w0)
          (w2, h2) = (w1 {c = succ (c w1)}, h1 S.|> w1)
       in exec xs (w2 {x = x w2 + y}, h2)
    exec (Nothing : xs) (w, h) =
      let w0 = w
          h0 = h
       in exec xs (w0 {c = succ (c w)}, h0 S.|> w0)

part1 :: Input -> Result
part1 i =
  let h = execute i
   in sum $ map (\n -> let w = h !! n in x w * c w) [20, 60, 100, 140, 180, 220]

part2 :: Input -> Result
part2 i = trace output 0
  where
    h = tail $ execute i
    draw _ [] out = toList out
    draw n (w : ws) out =
      let out' =
            if n >= x w - 1 && n <= x w + 1
              then out S.|> '#'
              else out S.|> '.'
       in draw ((n + 1) `mod` 40) ws out'
    output = unlines $ chunksOf 40 (draw 0 h S.empty)

solve' :: (Input -> Result) -> T.Text -> Result
solve' part = solve part pInput

part1Sample :: Result
part1Sample = solve' part1 sample

part1Input :: Result
part1Input = solve' part1 input

part2Sample :: Result
part2Sample = solve' part2 sample

part2Input :: Result
part2Input = solve' part2 input
