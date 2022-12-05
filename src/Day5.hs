{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day5 (part1, part2, input, sample) where

import Control.Monad (void)
import qualified Data.HashMap.Lazy as HM
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unlines)
import Lib (Parser, doParse, fetch)
import Text.Megaparsec
  ( MonadParsec (try),
    between,
    choice,
    sepBy,
    some,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    newline,
    string,
  )
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (unlines)

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
  unlines
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

pInstruction :: Parser Instruction
pInstruction = do
  void $ string "move "
  count <- decimal
  void $ string " from "
  from <- decimal
  void $ string " to "
  to <- decimal
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
      mapping' = foldl (moveCrates order) mapping instructions
   in pack $ map (head . (mapping' HM.!)) stackNumbers

part1 :: Text -> Text
part1 = solve reverse

part2 :: Text -> Text
part2 = solve id
