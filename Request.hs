{-# LANGUAGE OverloadedStrings #-}

module Request where

import Prelude hiding (takeWhile)

import qualified Data.ByteString.Char8 as Bytes
import Data.Monoid

import Data.Attoparsec.ByteString.Char8

data Request
  = RequestGet FilePath
  deriving (Show)

serializeRequest :: Request -> Bytes.ByteString
serializeRequest (RequestGet path) = "Get: " <> (Bytes.pack path) <> "\r\n"

parseRequest :: Bytes.ByteString -> Maybe Request
parseRequest = (either (const Nothing) Just) . (parseOnly requestP)

requestP :: Parser Request
requestP = requestGetP

requestGetP :: Parser Request
requestGetP = (RequestGet . Bytes.unpack) <$> (string "Get: " *> takeWhile (/= '\r') <* crlf)

crlf :: Parser Bytes.ByteString
crlf = string "\r\n"
