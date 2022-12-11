module Day6 (input, checkPacket, checkPackets, tests) where

import Data.Text (unpack)
import Lib (fetch)
import RIO
import RIO.List
import RIO.List.Partial
import RIO.Partial (succ)

day :: Integer
day = 6

input :: String
input = unpack $ fetch day

tests :: [String]
tests =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

checkPacket :: Int -> String -> Maybe Int
checkPacket n = check 0
  where
    check idx list =
      let chunk = take n list
       in if
              | length chunk < n -> Nothing
              | nub chunk == chunk -> Just $ idx + n
              | otherwise -> check (succ idx) (tail list)

checkPackets :: Int -> [String] -> [(String, Maybe Int)]
checkPackets n = map (\x -> (x, checkPacket n x))
