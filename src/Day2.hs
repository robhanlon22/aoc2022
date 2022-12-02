module Day2 (part1, part2, input) where

import Data.Text (Text)
import Lib (Parser, doParse, fetch)

day :: Integer
day = 2

input :: Text
input = fetch day

pInput :: Parser a
pInput = undefined

parse :: Text -> a
parse = doParse pInput

part1 :: Text -> Integer
part1 = undefined . parse

part2 :: Text -> Integer
part2 = undefined . parse
