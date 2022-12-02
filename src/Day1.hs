module Day1 (part1, part2, input) where

import Data.Heap (MaxHeap, fromList, take)
import Data.Text (Text)
import Lib (doParse, fetch)
import Text.Megaparsec (endBy, sepBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (take)

input :: Text
input = fetch 1

-- | The input text consists of numbers followed by newlines, with each group
-- separated by a newline. The parser declared here mirrors that structure
-- exactly.
parse :: Num a => Text -> [[a]]
parse = doParse $ decimal `endBy` newline `sepBy` newline

-- | Given the elf data, build a heap of the sums of each elf.
heapify :: (Num a, Ord a) => [[a]] -> MaxHeap a
heapify = fromList . map sum

-- | Take the top N items from the elf heap.
sumTopN :: (Num a, Ord a) => Int -> [[a]] -> a
sumTopN = (sum .) . (. heapify) . take

-- | Part 1 sums the top 1, a.k.a. takes the max.
part1 :: (Num a, Ord a) => Text -> a
part1 = sumTopN 1 . parse

-- | Part 2 sums the top 3.
part2 :: (Num a, Ord a) => Text -> a
part2 = sumTopN 3 . parse
