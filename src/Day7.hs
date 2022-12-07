{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Control.Monad (void)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import Lib (Parser, fetch, solve)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead),
    choice,
    many,
    manyTill,
    (<|>),
  )
import Text.Megaparsec.Char (char, newline, printChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- import qualified Data.HashMap.Lazy as HM

type Result = Int

day :: Integer
day = 7

-- type Filesystem = HM.HashMap T.Text (HM.HashMap T.Text Child)

data Size = Unknown | Known Int deriving (Eq, Show)

data Child = Subdirectory Directory | File T.Text Int deriving (Eq, Show)

data Directory = Directory T.Text [Child] Size deriving (Eq, Show)

input :: T.Text
input = fetch day

sample :: T.Text
sample = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"

pName :: Parser T.Text
pName = T.pack <$> manyTill printChar newline

pCdParent :: Parser ()
pCdParent = do
  void $ string "$ cd .."
  void newline

pCdDir :: Parser Directory
pCdDir = do
  void $ string "$ cd "
  name <- pName
  children <- manyTill pChildren (pCdParent <|> eof)
  return $ Directory name (concat children) Unknown

pFile :: Parser Child
pFile = do
  size <- L.decimal
  void $ char ' '
  filename <- pName
  return $ File filename size

pIgnoreDir :: Parser ()
pIgnoreDir = do
  void $ string "dir "
  void pName

pLsOutput :: Parser (Maybe Child)
pLsOutput =
  choice
    [ Just <$> pFile,
      Nothing <$ pIgnoreDir
    ]

pLs :: Parser [Child]
pLs = do
  void $ string "$ ls"
  void newline
  children <- manyTill pLsOutput (void (lookAhead $ char '$') <|> eof)
  return $ catMaybes children

pSubdir :: Parser [Child]
pSubdir = do
  dir <- pCdDir
  return [Subdirectory dir]

pChildren :: Parser [Child]
pChildren = pSubdir <|> pLs

pInput :: Parser Directory
pInput = do
  void $ string "$ cd /"
  void newline
  children <- many pChildren
  return $ Directory "/" (concat children) Unknown

calculateSizes :: Directory -> Directory
calculateSizes (Directory name entries _) = Directory name entries' (Known size)
  where
    calc (Subdirectory d) = Subdirectory $ calculateSizes d
    calc e = e
    entries' = map calc entries
    size =
      sum $
        map
          ( \case
              Subdirectory (Directory _ _ (Known s)) -> s
              Subdirectory (Directory _ _ Unknown) -> error "oops"
              File _ s -> s
          )
          entries'

collectDirectories :: Directory -> [Directory]
collectDirectories dir@(Directory _ entries _) = dir : concatMap collect entries
  where
    collect (Subdirectory d) = collectDirectories d
    collect _ = []

part1 :: Directory -> Result
part1 i =
  sum
    $ mapMaybe
      ( \case
          (Directory _ _ (Known s)) -> if s <= 100000 then Just s else Nothing
          _ -> Nothing
      )
    $ collectDirectories
    $ calculateSizes i

part2 :: Directory -> Result
part2 i =
  let dirs = collectDirectories $ calculateSizes i
      rootSize = case head dirs of
        Directory _ _ (Known s) -> s
        _ -> error "ack"
      space = 70000000
      used = space - rootSize
      needed = 30000000
   in minimum
        $ mapMaybe
          ( \case
              (Directory _ _ (Known s)) ->
                if used + s >= needed then Just s else Nothing
              _ -> Nothing
          )
        $ tail dirs

solve' :: (Directory -> Result) -> T.Text -> Result
solve' part = solve part pInput

part1Sample :: Result
part1Sample = solve' part1 sample

part1Input :: Result
part1Input = solve' part1 input

part2Sample :: Result
part2Sample = solve' part2 sample

part2Input :: Result
part2Input = solve' part2 input
