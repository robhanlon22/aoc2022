{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day5 (part1, part2, input, sample) where

import Data.HashMap.Lazy qualified as HM
import Data.Text qualified as T
import Lib (Parser, doParse, fetch)
import RIO hiding (some, to, try)
import RIO.List
import RIO.List.Partial
import Text.Megaparsec
  ( MonadParsec (try),
    between,
    choice,
    manyTill_,
    sepBy,
    some,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    newline,
    string,
  )
import Text.Megaparsec.Char.Lexer (decimal)

data Instruction = Instruction
  { count, from, to :: Int
  }
  deriving (Eq, Show)

data Input = Input
  { crateLines :: [[Maybe Char]],
    stackNumbers :: [Int],
    instructions :: [Instruction]
  }
  deriving (Eq, Show)

day :: Integer
day = 5

input :: Text
input = fetch day

sample :: Text
sample =
  T.unlines
    [ "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 ",
      "",
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    ]

pCrate :: Parser (Maybe Char)
pCrate =
  choice
    [ Just <$> between (char '[') (char ']') alphaNumChar,
      Nothing <$ string "   "
    ]

pCrateLine :: Parser [Maybe Char]
pCrateLine = do
  crates <- pCrate `sepBy` char ' '
  void newline
  return crates

pCrateLines :: Parser [[Maybe Char]]
pCrateLines = some . try $ pCrateLine

pIgnore :: Parser ()
pIgnore = do
  void (letterChar <|> char ' ')

pInstruction :: Parser Instruction
pInstruction = do
  (_, count) <- manyTill_ pIgnore decimal
  (_, from) <- manyTill_ pIgnore decimal
  (_, to) <- manyTill_ pIgnore decimal
  void newline
  return $ Instruction {..}

pInstructions :: Parser [Instruction]
pInstructions = some pInstruction

pStackNumber :: Parser Int
pStackNumber = do
  void $ char ' '
  n <- decimal
  void $ char ' '
  return n

pStackNumbers :: Parser [Int]
pStackNumbers = do
  stackNumbers <- pStackNumber `sepBy` char ' '
  void newline
  return stackNumbers

pInput :: Parser Input
pInput = do
  crateLines <- pCrateLines
  stackNumbers <- pStackNumbers
  void newline
  instructions <- pInstructions
  return $ Input {..}

moveCrates :: ([Char] -> [Char]) -> HM.HashMap Int [Char] -> Instruction -> HM.HashMap Int [Char]
moveCrates order mapping (Instruction {..}) =
  let (xs, fromStack) = splitAt count (mapping HM.! from)
      toStack = order xs ++ (mapping HM.! to)
   in HM.insert to toStack $ HM.insert from fromStack mapping

solve :: ([Char] -> [Char]) -> Text -> Text
solve order i =
  let Input {..} = doParse pInput i
      stacks = map catMaybes $ transpose crateLines
      mapping = HM.fromList $ zip stackNumbers stacks
      mapping' = foldl' (moveCrates order) mapping instructions
   in T.pack $ map (head . (mapping' HM.!)) stackNumbers

part1 :: Text -> Text
part1 = solve reverse

part2 :: Text -> Text
part2 = solve id
