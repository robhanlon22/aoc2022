module Day14
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
import RIO.HashMap qualified as HM
import RIO.List
import RIO.Vector qualified as V
import RIO.Vector.Boxed qualified as VB
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- import Data.List.Split

day :: Integer
day = 14

input :: IO T.Text
input = fetchSafe day

sample :: IO T.Text
sample = return "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

type Input = [[(Int, Int)]]

type Result = Maybe Int

data Cell = Rock | Sand | Empty deriving (Eq)

instance Show Cell where
  show Rock = "#"
  show Sand = "o"
  show Empty = "."

pInput :: Parser Input
pInput =
  ( ( do
        lhs <- L.decimal
        void $ char ','
        rhs <- L.decimal
        return (lhs, rhs)
    )
      `sepBy` string " -> "
  )
    `endBy` newline

buildGrid :: Input -> HashMap (Int, Int) Cell
buildGrid =
  foldl'
    ( \a row ->
        case tailMaybe row of
          Just tail ->
            foldl'
              ( \b ((x1, y1), (x2, y2)) ->
                  HM.union
                    b
                    ( HM.fromList
                        ( map
                            (,Rock)
                            ( if x1 == x2
                                then [(x1, y) | y <- if y1 < y2 then [y1 .. y2] else [y2 .. y1]]
                                else [(x, y1) | x <- if x1 < x2 then [x1 .. x2] else [x2 .. x1]]
                            )
                        )
                    )
              )
              a
              (zip row tail)
          Nothing -> a
    )
    HM.empty

dropSand1 world = do
  abyssBegins <- maximumMaybe $ map snd $ HM.keys world
  let source = (500, 0)
      d w (x, y)
        | y == abyssBegins = w
        | not (HM.member (x, y + 1) w) = d w (x, y + 1)
        | not (HM.member (x - 1, y + 1) w) = d w (x - 1, y + 1)
        | not (HM.member (x + 1, y + 1) w) = d w (x + 1, y + 1)
        | otherwise = d (HM.insert (x, y) Sand w) source
   in return $ d world source

dropSand2 world = do
  bottomRock <- maximumMaybe $ map snd $ HM.keys world
  let floorTile = bottomRock + 2
  let source = (500, 0)
      d w s@(x, y)
        | y + 1 == floorTile = d (HM.insert (x, y) Sand w) source
        | not (HM.member (x, y + 1) w) = d w (x, y + 1)
        | not (HM.member (x - 1, y + 1) w) = d w (x - 1, y + 1)
        | not (HM.member (x + 1, y + 1) w) = d w (x + 1, y + 1)
        | s == source = HM.insert (x, y) Sand w
        | otherwise = d (HM.insert (x, y) Sand w) source
   in return $ d world source

part1 :: Input -> Result
part1 ipt = do
  result <- dropSand1 (buildGrid ipt)
  return $ countBy (== Sand) $ HM.elems result

part2 :: Input -> Result
part2 ipt = do
  result <- dropSand2 (buildGrid ipt)
  return $ countBy (== Sand) $ HM.elems result

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
