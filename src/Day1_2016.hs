module Day1_2016 where

import Control.Lens
import Data.Text qualified as T
import Lib (Parser, ParserError, ParserResult, fetchYearDay, solve3)
import Network.Curl
import RIO
import RIO.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- newtype CurlError = CurlError (CurlCode, String)

-- makeClassyPrisms ''CurlError
-- makeClassyPrisms ''ParseErrorBundle

-- input :: (AsCurlError err) => IO (Either err Text)
-- input = do
--   response <- fetchYearDay 2016 1
--   return $ case response of
--     Left e -> Left $ CurlError e
--     Right r -> Right r

-- data Dir = UpDir | DownDir | LeftDir | RightDir

-- data Turn = LeftTurn | RightTurn

-- data Instruction = Instruction Turn Int

-- type Input = [Instruction]

-- type Result = Int

-- pInput :: Parser Input
-- pInput =
--   do
--     do
--       turn <- RightTurn <$ char 'R' <|> LeftTurn <$ char 'L'
--       Instruction turn <$> L.decimal
--     `sepBy` (void $ string ", ")

-- part1 :: Input -> Result
-- part1 ipt = undefined

-- part2 :: Input -> Result
-- part2 ipt = undefined

-- solve' :: (AsCurlError err, AsParseErrorBundle err Text Void) => (Input -> Result) -> FetchResponse -> Either err Result
-- solve' part response = do
--   text <- response
--   ipt <- runParser pInput "" text
--   return $ part ipt

-- solve :: (AsCurlError err1, AsCurlError err2, AsParseErrorBundle err2 Text Void) => (Input -> Result) -> IO (FetchResponse  -> IO (Either err Result)
-- solve part responseIO = solve' part <$> responseIO

-- part1Input :: (AsCurlError err, AsParseErrorBundle err Text Void) => IO (Either err Result)
-- part1Input = solve part1 input

-- part2Input :: (AsCurlError err, AsParseErrorBundle err Text Void) => IO (Either err Result)
-- part2Input = solve part2 input
