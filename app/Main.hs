module Main (main) where

import qualified Day1
import System.Environment (getArgs)

main' :: [String] -> IO ()
main' ["1", "1"] = Day1.part1 >>= print
main' ["1", "2"] = Day1.part2 >>= print
main' args = error $ "Unknown args: " ++ show args

main :: IO ()
main = getArgs >>= main'
