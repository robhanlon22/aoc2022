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

type Input = [(Packet, Packet)]

type Result = Maybe Int

data Packet = Literal Int | Packet [Packet] deriving (Eq, Show)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (Packet []) (Packet []) = EQ
  compare (Packet []) _ = LT
  compare _ (Packet []) = GT
  compare (Literal a) (Literal b) = a `compare` b
  compare lhs@(Packet _) rhs@(Literal _) = lhs `compare` Packet [rhs]
  compare lhs@(Literal _) rhs@(Packet _) = Packet [lhs] `compare` rhs
  compare (Packet (x : xs)) (Packet (y : ys)) =
    case x `compare` y of
      EQ -> Packet xs `compare` Packet ys
      r -> r

pPacket :: Parser Packet
pPacket =
  Packet
    <$> between
      (char '[')
      (char ']')
      ( choice
          [ Literal <$> L.decimal,
            pPacket
          ]
          `sepBy` char ','
      )

pPair :: Parser (Packet, Packet)
pPair = do
  lhs <- pPacket
  void newline
  rhs <- pPacket
  void newline
  return (lhs, rhs)

pInput :: Parser [(Packet, Packet)]
pInput = pPair `sepBy` newline <* eof

part1 :: Input -> Result
part1 =
  return
    . sum
    . catMaybes
    . zipWith
      (\i (lhs, rhs) -> if lhs < rhs then Just i else Nothing)
      [1 ..]

dividerPacket :: Int -> Packet
dividerPacket n = Packet [Packet [Literal n]]

twoDivider :: Packet
twoDivider = dividerPacket 2

sixDivider :: Packet
sixDivider = dividerPacket 6

part2 :: Input -> Result
part2 ipt =
  let packets =
        sort $
          twoDivider : sixDivider : concatMap (\(a, b) -> [a, b]) ipt
   in do
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
