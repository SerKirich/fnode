module Server where

import qualified Data.ByteString.Char8 as Bytes

import Data.List (isPrefixOf)

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Fix

import Control.Concurrent
import Control.Exception

import Request
import Responce

runServer :: MVar Bool -> [FilePath] -> ServiceName -> IO ()
runServer exit shared port = do
  addr <- getServerAddr port
  bracket (start addr) stop $ \sock -> forever $ do
    e <- readMVar exit
    when e $ return undefined
    (conn, _) <- accept sock
    void $ forkFinally (handleConnection conn shared) (disconnect conn)

handleConnection :: Socket -> [FilePath] -> IO ()
handleConnection conn shared = do
  request <- recvRequest conn
  case request of
    Nothing -> sendResponce conn $ ResponceError InvalidRequestError
    Just (RequestGet path) -> do
      if any (`isPrefixOf` path) shared
        then do
          isFile <- doesFileExist path
          if isFile
            then do
              b <- Bytes.readFile path
              sendResponce conn $ ResponceFile b
            else do
              isDir <- doesDirectoryExist path
              if isDir
                then do
                  (ds, fs) <- getDirectoryList path
                  sendResponce conn $ ResponceList ds fs
                else sendResponce conn $ ResponceError NotFoundError
        else sendResponce conn $ ResponceError NotFoundError

disconnect :: Socket -> Either SomeException () -> IO ()
disconnect conn (Left  _) = (sendResponce conn $ ResponceError InternalError) >> close conn
disconnect conn (Right _) =                                                      close conn

start :: AddrInfo -> IO Socket
start addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock $ addrAddress addr
  listen sock 8
  return sock

stop :: Socket -> IO ()
stop = close

getServerAddr :: ServiceName -> IO AddrInfo
getServerAddr port = do
  let hints = defaultHints {
    addrFlags = [AI_PASSIVE],
    addrSocketType = Stream
  }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

recvRequest :: Socket -> IO (Maybe Request)
recvRequest conn = do
  req <- recv conn 1024
  return $ parseRequest req

sendResponce :: Socket -> Responce -> IO ()
sendResponce conn res = do
  let b = serializeResponce res
  let n = (Bytes.length b `div` 1024) + 1
  sendAll conn $ Bytes.pack $ (show n) ++ "\r\n"
  void $ (\f -> foldM f b [1..n]) $ \a _ -> do
    sendAll conn $ Bytes.take 1024 a
    return $ Bytes.drop 1024 a

getDirectoryList :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryList path = do
  list <- ((map (path </>)) <$> listDirectory path)
  ds <- filterM doesDirectoryExist list
  fs <- filterM doesFileExist      list
  return (map takeFileName ds, map takeFileName fs)
