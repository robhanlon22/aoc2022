module Day15
  ( part1Sample,
    part1Input,
    part2Sample,
    part2Input,
    input,
    sample,
  )
where

import Data.Range
import Data.Text qualified as T
import Data.Text.IO (readFile)
import Lib
import RIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- import Data.List.Split

day :: Integer
day = 15

input :: IO T.Text
input = fetchSafe day

sample :: IO T.Text
sample = readFile "2022_15_sample.txt"

type Point = (Integer, Integer)

type Result = Maybe Integer

signedInteger :: Parser Integer
signedInteger =
  L.signed (return ()) L.decimal

type Input = [(Point, Point)]

pInput :: Parser Input
pInput =
  ( do
      void $ string "Sensor at x="
      sensorX <- signedInteger
      void $ string ", y="
      sensorY <- signedInteger
      void $ string ": closest beacon is at x="
      beaconX <- signedInteger
      void $ string ", y="
      beaconY <- signedInteger
      return ((sensorX, sensorY), (beaconX, beaconY))
  )
    `endBy` newline

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

ranges :: Integer -> Integer -> Integer -> Input -> [Range Integer]
ranges row minX maxX =
  foldl'
    ( \noBeacon (sensor@(sensorX, sensorY), beacon) ->
        let distance = manhattan beacon sensor
            vertical = abs (sensorY - row)
            diff = vertical - distance
         in if diff > 0
              then noBeacon
              else
                noBeacon
                  `union` [ max minX (sensorX - diff)
                              +=+ min (sensorX + diff) maxX
                          ]
    )
    []

part1 :: Integer -> Input -> Result
part1 row ipt =
  Just $
    sum $
      mapMaybe
        ( \case
            SpanRange (Bound lhs Inclusive) (Bound rhs Inclusive) -> Just $ abs (lhs - rhs)
            SingletonRange x -> Just x
            _ -> Nothing
        )
        (ranges row (-100000000000) 100000000000 ipt)

part2 :: Integer -> Input -> Result
part2 maxRow ipt = p 0
  where
    p row = do
      guard $ row <= maxRow
      let rs = ranges row 0 maxRow ipt
      if length rs > 1
        then case rs of
          [ SpanRange _ (Bound x1 Inclusive),
            SpanRange (Bound x2 Inclusive) _
            ] -> return $ ((x1 + x2) `div` 2) * 4000000 + row
          _ -> fail "Range was in unexpected format"
        else p (row + 1)

solve' :: (Input -> Result) -> IO Text -> IO (ParserResult Result)
solve' = solve3 pInput

part1Sample :: IO (ParserResult Result)
part1Sample = solve' (part1 10) sample

part1Input :: IO (ParserResult Result)
part1Input = solve' (part1 2000000) input

part2Sample :: IO (ParserResult Result)
part2Sample = solve' (part2 20) sample

part2Input :: IO (ParserResult Result)
part2Input = solve' (part2 4000000) input
