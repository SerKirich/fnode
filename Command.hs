{-# LANGUAGE OverloadedStrings #-}

module Command where

import Prelude hiding (takeWhile)

import qualified Data.ByteString.Char8 as Bytes
import Data.Monoid

import Data.Attoparsec.ByteString.Char8 hiding (isSpace)
import Control.Applicative

import Data.Char

data Command
  = Get FilePath
  | Exit
  deriving (Show)

serializeCommand :: Command -> Bytes.ByteString
serializeCommand (Get path) = "Get: " <> Bytes.pack path <> "\r\n"

parseCommand :: String -> Maybe Command
parseCommand = (either (const Nothing) Just) . (parseOnly commandP) . Bytes.pack

commandP :: Parser Command
commandP = getP <|> exitP

getP :: Parser Command
getP = Get <$> (string "get " *> wordP)

exitP :: Parser Command
exitP = Exit <$ string "exit"

wordP :: Parser String
wordP = quotedWordP <|> regularWordP

quotedWordP :: Parser String
quotedWordP = toString $ char8 '"' *> takeWhile (/= '"') <* char8 '"'

regularWordP :: Parser String
regularWordP = many1 $ satisfy (\c -> (not $ isSpace c) && (c /= '"'))

toString :: Parser Bytes.ByteString -> Parser String
toString p = Bytes.unpack <$> p
