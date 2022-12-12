module Day12 (part1Sample, part1Input, part2Sample, part2Input, input, sample) where

import Algorithm.Search
import Data.Text qualified as T
import Lib
import RIO
import RIO.HashMap qualified as HM
import RIO.List
import RIO.Partial
import Text.Megaparsec
import Text.Megaparsec.Char

-- import Data.List.Split

day :: Integer
day = 12

input :: IO T.Text
input = fetchSafe day

sample :: IO T.Text
sample = return "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi\n"

type Input = [[Mark]]

data Mark
  = Start
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | End
  deriving (Eq, Show, Ord, Enum)

type Result = Maybe Int

canStepTo :: Mark -> Mark -> Bool
canStepTo l r = r < l || l == r || succ l == r

pMark :: Parser Mark
pMark =
  choice
    [ Start <$ char 'S',
      A <$ char 'a',
      B <$ char 'b',
      C <$ char 'c',
      D <$ char 'd',
      E <$ char 'e',
      F <$ char 'f',
      G <$ char 'g',
      H <$ char 'h',
      I <$ char 'i',
      J <$ char 'j',
      K <$ char 'k',
      L <$ char 'l',
      M <$ char 'm',
      N <$ char 'n',
      O <$ char 'o',
      P <$ char 'p',
      Q <$ char 'q',
      R <$ char 'r',
      S <$ char 's',
      T <$ char 't',
      U <$ char 'u',
      V <$ char 'v',
      W <$ char 'w',
      X <$ char 'x',
      Y <$ char 'y',
      Z <$ char 'z',
      End <$ char 'E'
    ]

pMarks :: Parser [Mark]
pMarks = manyTill pMark (lookAhead newline)

pInput :: Parser Input
pInput = pMarks `endBy` newline

deltas :: [(Int, Int)]
deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x, y) = map (bimap (x +) (y +)) deltas

structures :: [[Mark]] -> ([((Int, Int), Mark)], HashMap (Int, Int) [(Int, Int)])
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
          ( \k mark ->
              mapMaybe
                ( \c -> do
                    v <- HM.lookup c m
                    guard $ mark `canStepTo` v
                    return c
                )
                (neighborCoords k)
          )
          m
   in (tuples, graph)

transitionCost :: a -> b -> Int
transitionCost = const . const 1

minCost :: (Ord k, Hashable k) => HashMap k [k] -> k -> k -> Maybe Int
minCost graph start end =
  fst
    <$> dijkstra
      (\k -> fromMaybe [] $ HM.lookup k graph)
      transitionCost
      (== end)
      start

findCoordsForValue :: Eq a => a -> [(b, a)] -> [b]
findCoordsForValue value =
  mapMaybe
    ( \(coord, v) -> do
        guard $ v == value
        return coord
    )

findCoordForValue :: Eq a1 => a1 -> [(a2, a1)] -> Maybe a2
findCoordForValue value tuples = headMaybe $ findCoordsForValue value tuples

findEnd :: [(a, Mark)] -> Maybe a
findEnd = findCoordForValue End

part1 :: Input -> Result
part1 ipt = do
  start <- findCoordForValue Start tuples
  end <- findEnd tuples
  minCost graph start end
  where
    (tuples, graph) = structures ipt

part2 :: Input -> Result
part2 ipt = do
  end <- findEnd tuples
  minimumMaybe $ mapMaybe (\start -> minCost graph start end) starts
  where
    (tuples, graph) = structures ipt
    starts = findCoordsForValue A tuples

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
