{-# LANGUAGE DuplicateRecordFields #-}

module Day7 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Data.HashMap.Lazy qualified as HM
import Data.Sequence qualified as S
import Data.Text qualified as T
import Lib (Parser, fetch, solve)
import RIO hiding (try)
import RIO.List.Partial
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, printChar, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Result = Integer

day :: Integer
day = 7

data Command
  = CdCommand CdDirectory
  | LsCommand
  deriving (Eq, Show)

data CdDirectory = CdRelative T.Text | CdRoot | CdParent
  deriving (Eq, Show)

data Output
  = DirectoryOutput T.Text
  | FileOutput Integer T.Text
  deriving (Eq, Show)

data TerminalLine
  = CommandTerminalLine Command
  | OutputTerminalLine Output
  deriving (Eq, Show)

newtype World = World [TerminalLine]
  deriving (Eq, Show)

newtype FileNode = FileNode Integer
  deriving (Eq, Show)

newtype DirectoryNode = DirectoryNode (HM.HashMap T.Text FsNode)
  deriving (Eq, Show)

data FsNode
  = DirectoryFsNode DirectoryNode
  | FileFsNode FileNode
  deriving (Eq, Show)

input :: T.Text
input = fetch day

sample :: T.Text
sample = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"

pName :: Parser T.Text
pName = T.pack <$> manyTill printChar (lookAhead newline)

tryChoice :: [Parser a] -> Parser a
tryChoice ps = choice (map try ps)

pCdCommand :: Parser Command
pCdCommand =
  commandParser
    "cd "
    ( CdCommand
        <$> choice
          [ CdRoot <$ char '/',
            CdParent <$ string "..",
            CdRelative <$> pName
          ]
    )

pLsCommand :: Parser Command
pLsCommand = commandParser "ls" (return LsCommand)

commandParser :: T.Text -> Parser Command -> Parser Command
commandParser command parser = do
  void $ string "$ "
  void $ string command
  parser

pCommand :: Parser Command
pCommand =
  tryChoice
    [ pCdCommand,
      pLsCommand
    ]

pCommandTerminalLine :: Parser TerminalLine
pCommandTerminalLine = CommandTerminalLine <$> pCommand

pFileOutput :: Parser Output
pFileOutput = do
  size <- L.decimal
  void $ char ' '
  FileOutput size <$> pName

pDirectoryOutput :: Parser Output
pDirectoryOutput = do
  void $ string "dir "
  DirectoryOutput <$> pName

pOutput :: Parser Output
pOutput = tryChoice [pFileOutput, pDirectoryOutput]

pOutputTerminalLine :: Parser TerminalLine
pOutputTerminalLine = OutputTerminalLine <$> pOutput

pTerminalLine :: Parser TerminalLine
pTerminalLine = tryChoice [pCommandTerminalLine, pOutputTerminalLine]

pWorld :: Parser World
pWorld = World <$> pTerminalLine `endBy` newline

newFs :: DirectoryNode
newFs = DirectoryNode HM.empty

addNode :: DirectoryNode -> FsNode -> T.Text -> S.Seq T.Text -> DirectoryNode
addNode fs@(DirectoryNode children) node name path =
  let insertChild k v = HM.insert k v children
   in case S.viewl path of
        S.EmptyL ->
          let updated = DirectoryNode $ insertChild name node
           in case node of
                DirectoryFsNode _ ->
                  case children HM.!? name of
                    Just (DirectoryFsNode _) -> fs
                    _ -> updated
                _ -> updated
        p S.:< ps ->
          let child = case children HM.!? p of
                Just (DirectoryFsNode d) -> d
                _ -> newFs
              directory = addNode child node name ps
           in DirectoryNode $ insertChild p (DirectoryFsNode directory)

buildFs :: World -> DirectoryNode
buildFs (World terminalLines) = build terminalLines newFs S.empty
  where
    build [] fs _ = fs
    build (tl : tls) fs@(DirectoryNode _) path =
      let addDirectoryNode name = addNode fs (DirectoryFsNode newFs) name path
       in case tl of
            CommandTerminalLine command ->
              case command of
                LsCommand -> build tls fs path
                CdCommand directory ->
                  case directory of
                    CdRoot ->
                      build tls fs S.empty
                    CdParent ->
                      build
                        tls
                        fs
                        ( case S.viewr path of
                            (xs S.:> _) -> xs
                            _ -> path
                        )
                    CdRelative name ->
                      build tls (addDirectoryNode name) (path S.|> name)
            OutputTerminalLine output ->
              case output of
                DirectoryOutput name ->
                  build tls (addDirectoryNode name) path
                FileOutput size name ->
                  let fs' = addNode fs (FileFsNode $ FileNode size) name path
                   in build tls fs' path

data DirectorySize = DirectorySize Integer [DirectorySize] deriving (Eq, Show)

calculateSizes :: DirectoryNode -> DirectorySize
calculateSizes (DirectoryNode children) =
  foldl'
    ( \(DirectorySize size childSizes) child ->
        case child of
          DirectoryFsNode directory ->
            let childSize@(DirectorySize size' _) = calculateSizes directory
             in DirectorySize (size + size') (childSize : childSizes)
          FileFsNode (FileNode size') ->
            DirectorySize (size + size') childSizes
    )
    (DirectorySize 0 [])
    children

flattenSizes :: DirectorySize -> [Integer]
flattenSizes (DirectorySize size children) = size : concatMap flattenSizes children

part1 :: World -> Result
part1 =
  sum . filter (<= 100000) . flattenSizes . calculateSizes . buildFs

space :: Integer
space = 70000000

needed :: Integer
needed = 30000000

part2 :: World -> Result
part2 i =
  let (DirectorySize size children) = calculateSizes $ buildFs i
      used = space - size
   in minimum $
        filter (\s -> used + s >= needed) $
          concatMap flattenSizes children

solve' :: (World -> Result) -> T.Text -> Result
solve' part = solve part pWorld

part1Sample :: Result
part1Sample = solve' part1 sample

part1Input :: Result
part1Input = solve' part1 input

part2Sample :: Result
part2Sample = solve' part2 sample

part2Input :: Result
part2Input = solve' part2 input
