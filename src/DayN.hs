module DayN (part1, part2, input) where

import Data.Text (Text)
import Lib (Parser, doParse, fetch)

day :: Integer
day = undefined

input :: Text
input = fetch day

pInput :: Parser a
pInput = undefined

parse :: Text -> a
parse = doParse pInput

part1 :: Text -> a
part1 = undefined . parse

part2 :: Text -> a
part2 = undefined . parse
