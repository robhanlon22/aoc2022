{-# LANGUAGE RecordWildCards #-}

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
import RIO.Process
import RIO.Seq qualified as S
import RIO.Text qualified as T
import RIO.Vector qualified as V
import RIO.Vector.Boxed qualified as VB
import RIO.Vector.Partial as VP
import System.Environment
import Text.Megaparsec
  ( MonadParsec (lookAhead),
    sepBy,
    skipManyTill,
  )
import Text.Megaparsec.Char (char, newline, printChar, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Input = VB.Vector Monkey

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

data App = App
  { appIdx :: !(IORef Int),
    appMonkeys :: !(IORef Input),
    appRound :: !(IORef Integer),
    appMitigateWorry :: Integer -> Integer,
    appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL =
    lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL =
    lens
      appProcessContext
      (\x y -> x {appProcessContext = y})

class HasIdx app where
  getIdx :: app -> RIO App Int
  setIdx :: app -> Int -> RIO App ()

instance HasIdx App where
  getIdx = liftIO . readIORef <$> appIdx
  setIdx app = liftIO . writeIORef (appIdx app)

class HasRound app where
  getRound :: app -> RIO App Integer
  setRound :: app -> Integer -> RIO App ()

instance HasRound App where
  getRound = liftIO . readIORef <$> appRound
  setRound app = liftIO . writeIORef (appRound app)

class HasMonkeys app where
  getMonkeys :: app -> RIO App (VB.Vector Monkey)
  setMonkeys :: app -> VB.Vector Monkey -> RIO App ()

instance HasMonkeys App where
  getMonkeys = liftIO . readIORef <$> appMonkeys
  setMonkeys app = liftIO . writeIORef (appMonkeys app)

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

monkeyRound :: RIO App ()
monkeyRound = do
  app <- ask
  setIdx app 0
  round <- getRound app
  setRound app $ succ round
  tossItems
  where
    tossItems =
      do
        app <- ask
        idx <- getIdx app
        monkeys <- getMonkeys app
        if idx >= V.length monkeys
          then return ()
          else
            let srcMonkey@Monkey {items = srcItems} = monkeys VP.! idx
             in case S.viewl srcItems of
                  S.EmptyL -> do
                    setIdx app (succ idx)
                    tossItems
                  (old S.:< olds) ->
                    tossItem old olds srcMonkey
    tossItem old olds srcMonkey@Monkey {..} =
      do
        app <- ask
        idx <- getIdx app
        monkeys <- getMonkeys app
        let new = appMitigateWorry app $ applyOperation old operation
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
        setMonkeys app (monkeys VP.// updates)
        tossItems

runMonkeyBusiness :: (Integer -> Integer) -> Int -> Input -> IO Input
runMonkeyBusiness appMitigateWorry rounds i = do
  appIdx <- newIORef 0
  appMonkeys <- newIORef i
  appRound <- newIORef 0
  verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions <- logOptionsHandle stderr verbose
  withLogFunc logOptions $ \appLogFunc -> do
    appProcessContext <- mkDefaultProcessContext
    runRIO App {..} $ replicateM_ rounds monkeyRound
    readIORef appMonkeys

monkeyBusiness :: (Integer -> Integer) -> Int -> VB.Vector Monkey -> IO Int
monkeyBusiness mitigateWorry rounds i = do
  result <- runMonkeyBusiness mitigateWorry rounds i
  return $
    product $
      take 2 $
        reverse $
          sort $
            map
              (\Monkey {..} -> inspections)
              (toList result)

part1 :: Input -> IO Int
part1 = monkeyBusiness (`div` 3) 20

part2 :: Input -> IO Int
part2 i = monkeyBusiness (`mod` modulus) 10000 i
  where
    modulus = product $ V.map (divisibleBy . test) i

solve' :: (Input -> IO Int) -> T.Text -> IO Int
solve' part = solve part pInput

part1Sample :: IO Int
part1Sample = solve' part1 sample

part1Input :: IO Int
part1Input = solve' part1 input

part2Sample :: IO Int
part2Sample = solve' part2 sample

part2Input :: IO Int
part2Input = solve' part2 input
