module Lib
  ( fetch,
    doParse,
    Parser,
    countBy,
    readFileUnsafe,
    solve,
    fetchSafe,
    solve2,
    solve3,
    ParserResult,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Curl (CurlCode (CurlOK), CurlOption (CurlCookie), curlGetString)
import RIO
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

type ParserResult a = Either ParserError a

-- | The URL for the Advent of Code input for a given day.
url :: String -> String
url dayStr = "https://adventofcode.com/2022/day/" ++ dayStr ++ "/input"

-- | Loads the saved session secret from disk and returns a 'CurlCookie' to be
-- used in an authenticated request.
readCookie :: IO CurlOption
readCookie = do
  token <- TIO.readFile "session.secret"
  return $ CurlCookie $ T.unpack $ "session=" <> token

-- | Fetches the input for a given day. Attempts to load cached input from disk.
-- If cached input is not present, load from the Advent of Code website.
fetchSafe :: Integer -> IO Text
fetchSafe day = do
  let dayStr = show day
  let filename = dayStr ++ ".txt"

  exists <- doesFileExist filename

  if exists
    then TIO.readFile filename
    else do
      cookie <- readCookie
      response <- curlGetString (url dayStr) [cookie]
      case response of
        (CurlOK, body) -> do
          let body' = T.pack body
          TIO.writeFile filename body'
          return body'
        _ -> error $ show response

readFileUnsafe :: FilePath -> Text
readFileUnsafe = unsafePerformIO . TIO.readFile

-- | For convenience, unsafely extract the Text from the result of fetchSafe.
-- Would never be used in production code, but this is Advent of Code!
fetch :: Integer -> Text
fetch = unsafePerformIO . fetchSafe

-- | A quick-and-dirty unsafe 'runParser' wrapper that returns the parsed result
-- or errors if no result is present.
doParse :: Parser a -> Text -> a
doParse parser t =
  case runParser parser "" t of
    Right v -> v
    Left e -> error $ show e

countBy :: (Foldable t1, Num b) => (t2 -> Bool) -> t1 t2 -> b
countBy f = foldl' (\count xs -> count + if f xs then 1 else 0) 0

solve :: (a -> b) -> Parser a -> Text -> b
solve part parser text = part $ doParse parser text

solve2 :: Parser a -> (a -> b) -> Text -> b
solve2 parser part text = part $ doParse parser text

solve3 :: Parser a -> (a -> b) -> IO Text -> IO (ParserResult b)
solve3 parser part text = do
  t <- text
  return $ part <$> runParser parser "" t
