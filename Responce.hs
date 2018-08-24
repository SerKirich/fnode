{-# LANGUAGE OverloadedStrings #-}

module Responce where

import Prelude hiding (takeWhile)

import qualified Data.ByteString.Char8 as Bytes
import Data.Monoid

import Data.Attoparsec.ByteString.Char8
import Control.Applicative

data Responce
  = ResponceFile  Bytes.ByteString
  | ResponceList  [FilePath] [FilePath]
  | ResponceError ErrorType
  deriving (Show)

data ErrorType
  = InvalidRequestError
  | NotFoundError
  | InternalError

instance Show ErrorType where
  show InvalidRequestError = "Invalid Request"
  show NotFoundError       = "Not Found"
  show InternalError       = "Internal Error"

serializeResponce :: Responce -> Bytes.ByteString
serializeResponce (ResponceFile  b) = "File\r\n\r\n" <> b
serializeResponce (ResponceError e) = "Error: "      <> serializeErrorType e <> "\r\n"
serializeResponce (ResponceList ds fs) = "List\r\n" <> (Bytes.concat $ map (\d -> "Dir  " <> Bytes.pack d <> "\r\n") ds)
                                                    <> (Bytes.concat $ map (\f -> "File " <> Bytes.pack f <> "\r\n") fs)

serializeErrorType :: ErrorType -> Bytes.ByteString
serializeErrorType e = Bytes.pack $ show e

parseResponce :: Bytes.ByteString -> Maybe Responce
parseResponce = (either (const Nothing) Just) . (parseOnly responceP)

responceP :: Parser Responce
responceP = responceFileP <|> responceListP <|> responceErrorP

responceFileP :: Parser Responce
responceFileP = ResponceFile <$> (string "File" *> crlf *> crlf *> takeByteString)

responceListP :: Parser Responce
responceListP = do
  string "List"
  crlf
  list <- many $ (,) <$> (string "Dir  " <|> string "File ") <*> (takeWhile (/= '\r') <* crlf)
  let ds = map (Bytes.unpack . snd) $ filter (\x -> fst x == "Dir  ") list
  let fs = map (Bytes.unpack . snd) $ filter (\x -> fst x == "File ") list
  return $ ResponceList ds fs

responceErrorP :: Parser Responce
responceErrorP = ResponceError <$> (string "Error: " *> errorTypeP <* crlf)

errorTypeP :: Parser ErrorType
errorTypeP =  (InvalidRequestError <$ string "Invalid Request")
          <|> (NotFoundError       <$ string "Not Found"      )
          <|> (InternalError       <$ string "Internal Error" )

crlf :: Parser Bytes.ByteString
crlf = string "\r\n"
