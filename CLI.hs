module CLI where

import System.Console.Haskeline

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent

runCLI :: MVar Bool -> Chan String -> Chan String -> IO ()
runCLI exit inPipe outPipe = runInputT defaultSettings $ cli exit inPipe outPipe

cli :: MVar Bool -> Chan String -> Chan String -> InputT IO ()
cli exit inPipe outPipe = do
  e <- liftIO $ readMVar exit
  unless e $ do
    input <- getInputLine "> "
    case input of
      Nothing -> liftIO $ modifyMVar_ exit (\_ -> return True)
      Just s  -> do
        liftIO $ writeChan outPipe s
        res <- liftIO $ readChan inPipe
        outputStrLn res
        cli exit inPipe outPipe
