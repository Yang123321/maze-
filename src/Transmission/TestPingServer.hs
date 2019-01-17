module Transmission.TestPingServer where

import Network

import Transmission.Command
import Control.Monad

import Control.Concurrent
import System.IO
import Data.Time

startServer :: IO ()
startServer = withSocketsDo $ do
  sock <- listenOn (PortNumber 45678)
  print $ "listening the prot 45678"
  forever $ do
    (handle,host,port) <- accept sock
    print "-----new user--------"
    forkFinally (deal handle) (\_ -> do
                                 print "connect cancel"
                                 hClose handle)
    -- forkF


deal :: Handle -> IO ()
deal handle = do
  hSetBuffering handle LineBuffering

  login handle

  receive handle

  return ()
  where
    receive handle = forever $ do

      msg <- hGetLine handle
      tim <- getCurrentTime
      let CtestPing tim' = read msg
      print "--- time :: client -> server --- "
      print $ diffUTCTime tim tim'


      tim'' <- getCurrentTime
      hPutStrLn handle $ show $ StestPing tim''


nameList = ["nice","godless"]

login :: Handle -> IO String
login handle = do

  name <- hGetLine handle
  print name
  case read name of
    CaskForLogin n -> if n `elem` nameList
                      then do
                        hPutStrLn handle $ show $ SloginFailed "name is used, you need change one!"
                        login handle
                      else do
                        hPutStrLn handle $ show $ SloginSuccess
                        return n
    _ -> login handle
