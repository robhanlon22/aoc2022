module Day13
  ( part1Sample,
    part1Input,
    part2Sample,
    part2Input,
    input,
    sample,
  )
where

import Data.Text qualified as T
import Lib
import RIO
import RIO.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- import Data.List.Split

day :: Integer
day = 13

input :: IO T.Text
input = fetchSafe day

sample :: IO T.Text
sample = return "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]\n"

type Input = [([Packet], [Packet])]

type Result = Maybe Int

data Packet = Literal Int | Packet [Packet] deriving (Eq, Show)

pPacket :: Parser [Packet]
pPacket =
  between
    (char '[')
    (char ']')
    ( choice
        [ Literal <$> L.decimal,
          Packet <$> pPacket
        ]
        `sepBy` char ','
    )

pPair :: Parser ([Packet], [Packet])
pPair = do
  lhs <- pPacket
  void newline
  rhs <- pPacket
  void newline
  return (lhs, rhs)

pInput :: Parser [([Packet], [Packet])]
pInput = pPair `sepBy` newline <* eof

ordered :: [Packet] -> [Packet] -> Ordering
ordered [] [] = EQ
ordered [] _ = LT
ordered _ [] = GT
ordered (x : xs) (y : ys) =
  case (x, y) of
    (Literal a, Literal b) ->
      if
          | a < b -> LT
          | a > b -> GT
          | otherwise -> ordered xs ys
    (Packet _, Literal b) -> ordered (x : xs) (Packet [Literal b] : ys)
    (Literal a, Packet _) -> ordered (Packet [Literal a] : xs) (y : ys)
    (Packet a, Packet b) ->
      case ordered a b of
        EQ -> ordered xs ys
        r -> r

part1 :: Input -> Result
part1 ipt =
  return $
    sum $
      catMaybes $
        zipWith
          (\(lhs, rhs) i -> if ordered lhs rhs == LT then Just i else Nothing)
          ipt
          [1 ..]

dividerPacket :: Int -> [Packet]
dividerPacket n = [Packet [Packet [Literal n]]]

twoDivider :: [Packet]
twoDivider = dividerPacket 2

sixDivider :: [Packet]
sixDivider = dividerPacket 6

part2 :: Input -> Result
part2 ipt = do
  let packets = sortBy ordered $ twoDivider : sixDivider : concatMap (\(a, b) -> [a, b]) ipt
  twoIndex <- elemIndex twoDivider packets
  sixIndex <- elemIndex sixDivider packets
  return $ (twoIndex + 1) * (sixIndex + 1)

solve' :: (Input -> Result) -> IO Text -> IO (ParserResult Result)
solve' = solve3 pInput

part1Sample :: IO (ParserResult Result)
part1Sample = solve' part1 sample

part1Input :: IO (ParserResult Result)
part1Input = solve' part1 input

part2Sample :: IO (ParserResult Result)
part2Sample = solve' part2 sample

part2Input :: IO (ParserResult Result)
part2Input = solve' part2 input
