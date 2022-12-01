{-# LANGUAGE OverloadedStrings #-}

module Lib (fetch, parse, Parser) where

import Data.Text (Text, pack)
import Data.Void (Void)
import Network.Curl (CurlCode (CurlOK), CurlOption (CurlCookie), curlGetString)
import System.Directory (doesFileExist)
import Text.Megaparsec (Parsec, runParser)

type Parser = Parsec Void Text

url :: String -> String
url dayStr = "https://adventofcode.com/2022/day/" ++ dayStr ++ "/input"

readCookie :: IO CurlOption
readCookie = do
  token <- readFile "session.secret"
  return $ CurlCookie $ "session=" ++ token

fetch :: Integer -> IO Text
fetch day = do
  let dayStr = show day
  let filename = dayStr ++ ".txt"

  exists <- doesFileExist filename

  pack
    <$> if exists
      then readFile filename
      else do
        cookie <- readCookie
        response <- curlGetString (url dayStr) [cookie]
        case response of
          (CurlOK, body) -> do
            writeFile filename body
            return body
          _ -> error $ show response

parse :: Parser a -> Text -> a
parse parser t =
  case runParser parser "" t of
    Right v -> v
    Left e -> error $ show e
