module Day11
  ( part1Sample,
    part1Input,
    part2Sample,
    part2Input,
    input,
    sample,
  )
where

import Lib (Parser, fetch, solve)
import RIO
import RIO.List
import RIO.Partial
import RIO.Seq qualified as S
import RIO.Text qualified as T
import RIO.Vector qualified as V
import RIO.Vector.Partial as VP
import Text.Megaparsec
  ( MonadParsec (lookAhead),
    sepBy,
    skipManyTill,
  )
import Text.Megaparsec.Char (char, newline, printChar, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Input = Vector Monkey

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

data Test = Test
  { divisibleBy :: Integer,
    ifTrue :: Int,
    ifFalse :: Int
  }
  deriving (Eq, Show)

data Monkey = Monkey
  { items :: S.Seq Integer,
    operation :: Operation,
    test :: Test,
    inspections :: Int
  }
  deriving (Eq, Show)

newtype App = App
  { appLogFunc :: LogFunc
  }

instance HasLogFunc App where
  logFuncL =
    lens appLogFunc (\x y -> x {appLogFunc = y})

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
  return Operation {lhs, operator, rhs}

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
  return Test {divisibleBy, ifTrue, ifFalse}

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
  return Monkey {inspections = 0, items, operation, test}

pInput :: Parser Input
pInput = V.fromList <$> (pMonkey `sepBy` newline)

applyOperation :: Integer -> Operation -> Integer
applyOperation old Operation {lhs, operator, rhs} =
  function (getVal lhs) (getVal rhs)
  where
    function = case operator of
      Add -> (+)
      Mul -> (*)
    getVal Old = old
    getVal (Val v) = v

getDestination :: Integer -> Test -> Int
getDestination new Test {divisibleBy, ifTrue, ifFalse} =
  if new `mod` divisibleBy == 0
    then ifTrue
    else ifFalse

tossItems ::
  (Foldable v, V.Vector v Monkey) =>
  (Integer -> Integer) ->
  Int ->
  v Monkey ->
  v Monkey
tossItems mitigateWorry = tossN
  where
    tossN idx monkeys =
      if idx >= length monkeys
        then monkeys
        else
          let srcMonkey@Monkey {items = srcItems} = monkeys VP.! idx
           in case S.viewl srcItems of
                S.EmptyL -> do
                  tossN (idx + 1) monkeys
                (old S.:< olds) ->
                  toss1 idx monkeys old olds srcMonkey
    toss1 idx monkeys old olds srcMonkey@Monkey {operation, test, inspections} =
      do
        let new = mitigateWorry $ applyOperation old operation
        let dest = getDestination new test
        let destMonkey@Monkey {items = destItems} = monkeys VP.! dest
        let updates =
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
        tossN idx (monkeys VP.// updates)

monkeyRound ::
  (Foldable v, V.Vector v Monkey) =>
  (Integer -> Integer) ->
  v Monkey ->
  v Monkey
monkeyRound mitigateWorry = tossItems mitigateWorry 0

logRound ::
  ( MonadReader env f,
    Show a,
    MonadIO f,
    Integral a,
    Foldable t,
    HasLogFunc env
  ) =>
  t Monkey ->
  a ->
  f ()
logRound monkeys thisRound =
  when
    (thisRound == 1 || thisRound == 20 || thisRound `mod` 1000 == 0)
    ( do
        logInfo $
          fromString $
            "Round: " ++ show thisRound
        logInfo $
          fromString $
            "Top monkeys: " ++ show (topMonkeys monkeys)
        logInfo $
          fromString $
            "Monkey business: " ++ show (calcMonkeyBusiness monkeys)
        logInfo ""
    )

runMonkeyBusiness :: (Integer -> Integer) -> Int -> Input -> RIO App Input
runMonkeyBusiness mitigateWorry rounds i = do
  foldl'
    ( \acc thisRound -> do
        monkeys <- monkeyRound mitigateWorry <$> acc
        logRound monkeys thisRound
        return monkeys
    )
    (return i)
    [1 .. rounds]

topMonkeys :: Foldable t => t Monkey -> [Int]
topMonkeys monkeys =
  take 2 $
    reverse $
      sort $
        map inspections (toList monkeys)

calcMonkeyBusiness :: Foldable t => t Monkey -> Int
calcMonkeyBusiness = product . topMonkeys

monkeyBusiness :: (Integer -> Integer) -> Int -> Input -> RIO App Int
monkeyBusiness mitigateWorry rounds i =
  calcMonkeyBusiness <$> runMonkeyBusiness mitigateWorry rounds i

part1 :: Input -> RIO App Int
part1 = monkeyBusiness (`div` 3) 20

part2 :: Input -> RIO App Int
part2 i = monkeyBusiness (`mod` modulus) 10000 i
  where
    modulus = product $ V.map (divisibleBy . test) i

solve' :: (Input -> RIO App Int) -> T.Text -> IO Int
solve' part i = do
  logOptions <- logOptionsHandle stderr False
  withLogFunc logOptions $ \appLogFunc -> do
    runRIO App {appLogFunc} $ solve part pInput i

part1Sample :: IO Int
part1Sample = solve' part1 sample

part1Input :: IO Int
part1Input = solve' part1 input

part2Sample :: IO Int
part2Sample = solve' part2 sample

part2Input :: IO Int
part2Input = solve' part2 input
