module Transmission.TestPingClient where

import Network.Socket

import Transmission.Command
import Control.Monad

import Control.Concurrent
import System.IO
import Data.Time

import Control.Concurrent



reslove host port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr


open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock



startClient :: IO ()
startClient = do
  withSocketsDo $ do
    addr <- reslove "127.0.0.1" "45678"
    sock <- open addr
    handle <- socketToHandle sock ReadWriteMode

    name <- longin handle

    deal handle

longin :: Handle -> IO String
longin handle = do
  putStrLn "input your name"
  putStrLn "请输入名字"

  name <- getLine
  hPutStrLn handle $ show $ CaskForLogin name

  msg <- hGetLine handle
  print msg
  case read msg of
    SloginSuccess -> return name
    SloginFailed errInfo -> do
      print errInfo
      longin handle
    _   -> longin handle





deal :: Handle -> IO ()
deal handle = do
  hSetBuffering handle LineBuffering
  receive handle
  where
    receive handle = forever $ do

      tim'' <- getCurrentTime
      hPutStrLn handle $ show $ CtestPing tim''


      msg <- hGetLine handle
      tim <- getCurrentTime
      let StestPing tim' = read msg

      print "--- time :: serever -> client --- "
      print $ diffUTCTime tim tim'


      print "--- time :: client -> serever -> client --- "
      print $ diffUTCTime tim tim''

      threadDelay $ 10^6
      -- threadD

















