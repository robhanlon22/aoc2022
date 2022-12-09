module Main (main) where

import Criterion.Main
import Data.Text
import qualified Day5
import qualified Day6
import qualified Day9
import Text.Pretty.Simple

day5 :: (Text -> Text) -> Integer -> Text
day5 f _ = f Day5.input

day6 :: Int -> Maybe Int
day6 = flip Day6.checkPacket Day6.input

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Day5"
        [ bench "part1" $ whnf (day5 Day5.part1) 0,
          bench "part2" $ whnf (day5 Day5.part2) 0
        ],
      bgroup
        "Day6"
        [ bench "part1" $ whnf day6 4,
          bench "part2" $ whnf day6 14
        ],
      bgroup
        "Day9"
        [ bench "part1" $ whnf (const Day9.part1Input) 0,
          bench "part2" $ whnf (const Day9.part2Input) 0
        ]
    ]
