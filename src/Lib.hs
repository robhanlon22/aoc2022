module Lib (fetchInput) where

import Network.Curl
import System.Directory

fetchInput :: Integer -> IO String
fetchInput day = do
  let d = show day
  let filename = d ++ ".txt"
  exists <- doesFileExist filename

  if exists
    then readFile filename
    else do
      token <- readFile "session.secret"
      response <-
        curlGetString
          ("https://adventofcode.com/2022/day/" ++ d ++ "/input")
          [CurlCookie $ "session=" ++ token]
      case response of
        (CurlOK, body) -> do
          writeFile (d ++ ".txt") body
          return body
        _ -> error $ show response
