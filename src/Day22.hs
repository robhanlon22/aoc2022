{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE RecordWildCards #-}

module Day22 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import qualified Data.Text as T
import Lib

-- import qualified Data.HashSet as HS
-- import qualified Data.Vector as V
-- import qualified Data.HashMap.Lazy as HM
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L
-- import Data.List.Split

data Input = Input {}

type Result = Int

day :: Integer
day = 22

input :: T.Text
input = fetch day

sample :: T.Text
sample = ""

pInput :: Parser Input
pInput = do
  return Input {}

part1 :: Input -> Result
part1 _i = undefined

part2 :: Input -> Result
part2 _i = undefined

solve' :: (Input -> Result) -> T.Text -> Result
solve' part = solve part pInput

part1Sample :: Result
part1Sample = solve' part1 sample

part1Input :: Result
part1Input = solve' part1 input

part2Sample :: Result
part2Sample = solve' part2 sample

part2Input :: Result
part2Input = solve' part2 sample
