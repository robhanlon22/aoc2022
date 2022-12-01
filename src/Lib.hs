{-# LANGUAGE OverloadedStrings #-}

module Lib (fetch, parse) where

import Data.Text (Text, pack)
import Data.Void (Void)
import Network.Curl
  ( CurlCode (CurlOK),
    CurlOption (CurlCookie),
    curlGetString,
  )
import System.Directory (doesFileExist)
import Text.Megaparsec
  ( Parsec,
    runParser,
  )

type Parser = Parsec Void Text

fetch :: Integer -> IO Text
fetch day = do
  let d = show day
  let filename = d ++ ".txt"
  exists <- doesFileExist filename

  if exists
    then pack <$> readFile filename
    else do
      token <- readFile "session.secret"
      response <-
        curlGetString
          ("https://adventofcode.com/2022/day/" ++ d ++ "/input")
          [CurlCookie $ "session=" ++ token]
      case response of
        (CurlOK, body) -> do
          writeFile (d ++ ".txt") body
          return $ pack body
        _ -> error $ show response

parse :: Parser a -> Text -> a
parse parser t =
  case runParser parser "" t of
    Right v -> v
    Left e -> error $ show e
