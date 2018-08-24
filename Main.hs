module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.IO

import Control.Concurrent

import Server
import Client
import CLI

main :: IO ()
main = do
  args <- getArgs
  if length args /= 3
    then usage
    else do
      shared <- getConfig
      exit <- newMVar False
      inPipe  <- newChan
      outPipe <- newChan
      forkIO $ runServer exit shared         (args !! 1)
      forkIO $ runClient exit inPipe outPipe (args !! 0) (args !! 2)
      runCLI exit outPipe inPipe

getConfig :: IO [FilePath]
getConfig = do
  home <- getHomeDirectory
  let path = home </> ".fnode"
  exists <- doesFileExist path
  if exists
    then readFile path >>= (return . filter (not . null) . lines)
    else do
      hPutStrLn stderr "Warning: cannot find .fnode config in home directory - you share no files!"
      return []

usage :: IO ()
usage = getProgName >>= (\name -> hPutStrLn stderr $ "Usage: " ++ name ++ " HOST SERVER_PORT CLIENT_PORT")
