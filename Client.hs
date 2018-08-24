module Client where

import qualified Data.ByteString.Char8 as Bytes

import Data.Monoid
import Data.Maybe

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

import System.FilePath

import Control.Monad
import Control.Monad.Fix

import Control.Concurrent
import Control.Exception

import Command
import Responce

runClient :: MVar Bool -> Chan String -> Chan String -> HostName -> ServiceName -> IO ()
runClient exit inPipe outPipe host port = do
  addr <- getClientAddr host port
  fix $ \loop -> do
    e <- readMVar exit
    unless e $ do
      command <- readChan inPipe
      case parseCommand command of
        Nothing   -> writeChan outPipe "Invalid command!" >> loop
        Just Exit -> writeChan outPipe "Bye!" >> modifyMVar_ exit (\_ -> return True)
        Just cmd@(Get path) -> bracket (open addr) close $ \conn -> do
          sendCommand conn cmd
          res <- recvResponce conn
          case res of
            ResponceFile  f     -> Bytes.writeFile (takeFileName path) f >> writeChan outPipe "Saved.\n"
            ResponceList  ds fs -> writeChan outPipe $ concat $ (map (\d -> "Directory | " ++ d ++ "\n") ds) ++ (map (\f -> "File      | " ++ f ++ "\n") fs)
            ResponceError e     -> writeChan outPipe $ "Error: " ++ (show e) ++ "\n"
          loop

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock

getClientAddr :: HostName -> ServiceName -> IO AddrInfo
getClientAddr host port = do
  let hints = defaultHints {
    addrSocketType = Stream
  }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr

recvResponce :: Socket -> IO Responce
recvResponce conn = do
  r <- recv conn 1024
  let n = read $ Bytes.unpack $ Bytes.takeWhile (/= '\r') r
  res <- (\f -> foldM f (Bytes.drop 1 $ Bytes.dropWhile (/= '\n') r) [1..n]) $ \a _ -> do
    b <- recv conn 1024
    return $ a <> b
  return $ fromJust $ parseResponce res

sendCommand :: Socket -> Command -> IO ()
sendCommand conn = (sendAll conn) . serializeCommand
