module Day12 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Algorithm.Search
import Data.Text qualified as T
import Lib
import RIO
import RIO.Char
import RIO.HashMap qualified as HM
import RIO.List
import Text.Megaparsec
import Text.Megaparsec.Char

-- import Data.List.Split

day :: Integer
day = 12

input :: IO T.Text
input = fetchSafe day

sample :: IO T.Text
sample = return "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi\n"

type Input = [[Int]]

type Result = Maybe Int

canStepTo :: (Num a, Ord a) => a -> a -> Bool
canStepTo l r = l == r || l + 1 == r || r < l

pMark :: Parser Int
pMark =
  choice
    [ 0 <$ char 'S',
      27 <$ char 'E',
      (\x -> ord x - ord 'a' + 1) <$> letterChar
    ]

pMarks :: Parser [Int]
pMarks = manyTill pMark (lookAhead newline)

pInput :: Parser Input
pInput = pMarks `endBy` newline

data Node = Node
  { value :: Int,
    neighbors :: [(Int, Int)]
  }
  deriving (Eq, Show)

deltas :: [(Int, Int)]
deltas = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x, y) =
  map
    (bimap (x +) (y +))
    deltas

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

structures :: [[Int]] -> ([((Int, Int), Int)], HashMap (Int, Int) Node)
structures ipt =
  let tuples =
        concat $
          zipWith
            ( \row x ->
                zipWith
                  (\col y -> ((x, y), col))
                  row
                  [0 :: Int ..]
            )
            ipt
            [0 :: Int ..]
      m = HM.fromList tuples
      graph =
        HM.mapWithKey
          ( \k value ->
              let neighbors =
                    mapMaybe
                      ( \c -> do
                          v <- HM.lookup c m
                          guard $ value `canStepTo` v
                          return c
                      )
                      (neighborCoords k)
               in Node {value = value, neighbors = neighbors}
          )
          m
   in (tuples, graph)

minCost :: HashMap (Int, Int) Node -> (Int, Int) -> (Int, Int) -> Maybe Int
minCost graph start end = do
  (cost, _) <-
    aStar
      ( \k -> case HM.lookup k graph of
          Just Node {neighbors} -> neighbors
          Nothing -> []
      )
      manhattan
      (manhattan end)
      (== end)
      start
  return cost

findCoordsForValue :: Eq a => a -> [(b, a)] -> [b]
findCoordsForValue value =
  mapMaybe
    ( \(coord, v) -> do
        guard $ v == value
        return coord
    )

findCoordForValue :: Eq a1 => a1 -> [(a2, a1)] -> Maybe a2
findCoordForValue value tuples = headMaybe $ findCoordsForValue value tuples

findEnd :: [(a, Int)] -> Maybe a
findEnd = findCoordForValue 27

part1 :: Input -> Result
part1 ipt = do
  let (tuples, graph) = structures ipt
  start <- findCoordForValue 0 tuples
  end <- findEnd tuples
  minCost graph start end

part2 :: Input -> Result
part2 ipt = do
  let (tuples, graph) = structures ipt
  let starts = findCoordsForValue 1 tuples
  end <- findEnd tuples
  let costs = mapMaybe (\start -> minCost graph start end) starts
  minimumMaybe costs

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
