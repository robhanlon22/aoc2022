{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Control.Monad (void)
import Data.Foldable (Foldable (toList))
import Data.List (sort)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Lib (Parser, fetch, solve)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Input = V.Vector Monkey

type Result = Int

day :: Integer
day = 11

input :: T.Text
input = fetch day

sample :: T.Text
sample = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1\n"

data Operand = Old | Val Integer deriving (Eq, Show)

data Operator = Add | Mul deriving (Eq, Show)

data Operation = Operation
  { operator :: Operator,
    lhs :: Operand,
    rhs :: Operand
  }
  deriving (Eq, Show)

data Test = Test {divisibleBy :: Integer, ifTrue :: Int, ifFalse :: Int} deriving (Eq, Show)

data Monkey = Monkey
  { items :: S.Seq Integer,
    operation :: Operation,
    test :: Test,
    inspections :: Int
  }
  deriving (Eq, Show)

pOperand :: Parser Operand
pOperand = Old <$ string "old" <|> Val <$> L.decimal

pFluff :: Parser a -> Parser ()
pFluff p = void $ skipManyTill printChar p

pIgnorePreamble :: Parser ()
pIgnorePreamble = void $ pFluff (lookAhead newline)

pItems :: Parser (S.Seq Integer)
pItems = do
  pFluff ": "
  S.fromList <$> L.decimal `sepBy` string ", "

pOperation :: Parser Operation
pOperation = do
  pFluff "= "
  lhs <- pOperand
  void $ char ' '
  operator <- Mul <$ char '*' <|> Add <$ char '+'
  void $ char ' '
  rhs <- pOperand
  return Operation {..}

pTestSide :: Parser Int
pTestSide = do
  pFluff "monkey "
  L.decimal

pTest :: Parser Test
pTest = do
  pFluff "by "
  divisibleBy <- L.decimal
  void newline
  ifTrue <- pTestSide
  void newline
  ifFalse <- pTestSide
  return Test {..}

pMonkey :: Parser Monkey
pMonkey = do
  pIgnorePreamble
  void newline
  items <- pItems
  void newline
  operation <- pOperation
  void newline
  test <- pTest
  void newline
  return Monkey {inspections = 0, ..}

pInput :: Parser Input
pInput = V.fromList <$> (pMonkey `sepBy` newline)

applyOperation :: Integer -> Operation -> Integer
applyOperation old Operation {..} = function (getVal lhs) (getVal rhs)
  where
    function = case operator of
      Add -> (+)
      Mul -> (*)
    getVal Old = old
    getVal (Val v) = v

getDestination :: Integer -> Test -> Int
getDestination new Test {..} =
  if new `mod` divisibleBy == 0
    then ifTrue
    else ifFalse

monkeyRound :: (Integer -> Integer) -> V.Vector Monkey -> V.Vector Monkey
monkeyRound mitigateWorry = tossItems 0
  where
    tossItems idx i | idx >= V.length i = i
    tossItems idx i =
      let srcMonkey@Monkey {items = srcItems} = i V.! idx
       in case S.viewl srcItems of
            S.EmptyL -> tossItems (succ idx) i
            (old S.:< olds) -> tossItem idx i old olds srcMonkey
    tossItem idx i old olds srcMonkey@Monkey {..} =
      let new = mitigateWorry $ applyOperation old operation
          dest = getDestination new test
          destMonkey@Monkey {items = destItems} = i V.! dest
          updates =
            [ ( idx,
                srcMonkey
                  { items = olds,
                    inspections = succ inspections
                  }
              ),
              ( dest,
                destMonkey
                  { items = destItems S.|> new
                  }
              )
            ]
       in tossItems idx (i V.// updates)

monkeyBusiness :: (Integer -> Integer) -> Int -> V.Vector Monkey -> Int
monkeyBusiness mitigateWorry rounds i =
  product $
    take 2 $
      reverse $
        sort $
          map
            (\Monkey {..} -> inspections)
            (toList $ iterate (monkeyRound mitigateWorry) i !! rounds)

part1 :: Input -> Result
part1 = monkeyBusiness (`div` 3) 20

part2 :: Input -> Result
part2 i = monkeyBusiness (`mod` modulus) 10000 i
  where
    modulus = product $ V.map (divisibleBy . test) i

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
