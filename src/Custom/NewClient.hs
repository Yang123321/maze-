{-# LANGUAGE RecordWildCards #-}
module Custom.NewClient where
import Server.Types
import Network.Socket

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception

import qualified Data.Map as M

import Control.Monad
import System.IO

import Data.Array.IArray ((!))

import Data.IORef

import Data.Maybe (maybe)
import Data.Array.IArray
import Transmission.Command

import Custom.NewType

import CreateMaze.Types
import Data.Binary
import Control.Concurrent.MVar


reslove host port = do
  let hints = defaultHints {addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr


open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock


startClient :: MVar String ->  MVar ClientState -> IORef String->  IO ()
startClient nameMVar clientStateMVar errorRef= do
  maze <- decodeFile "data/test/random/-3225345198293634463.mazeI" :: IO MazeI
  pos <- newIORef (150,150)
  h <- newIORef 100
  yourbullets <- newIORef []
  otherPlayers <- newIORef  []
  monsters <- newIORef  []
  lsIORef <- newIORef []
  operatesRef <- newEmptyMVar

  withSocketsDo $ do
    -- addr <- reslove "127.0.0.1" "45678"
    addr <- reslove "47.110.155.65" "45678"
    sock <- open addr
    handle <- socketToHandle sock ReadWriteMode
    name <- longin nameMVar errorRef handle

    let clientState = ClientState name pos h yourbullets otherPlayers monsters maze lsIORef handle operatesRef

    putMVar clientStateMVar clientState

    deal name clientState

longin :: MVar String -> IORef String-> Handle -> IO String
longin nameMVar errorRef handle = do
  name <- takeMVar nameMVar


  hPutStrLn handle $ show $ CaskForLogin name
  msg <- hGetLine handle
  print msg
  case read msg of
    SloginSuccess -> return name
    SloginFailed errInfo -> do
      print errInfo
      writeIORef errorRef errInfo
      longin nameMVar errorRef handle
    _   -> longin nameMVar errorRef handle





deal name clientState@ClientState{..}= do
  hSetBuffering connectServerHandler LineBuffering
  race (receive name clientState) (sendInformation name clientState)
  return ()

  where
    receive name ClientState{..}= forever $ do
      msg <- hGetLine connectServerHandler
      case read msg of
        SBodyList ls -> do
          (as,bs) <- foldM dd ([],[]) ls
          writeIORef playersPosition as
          writeIORef monsterList bs

        Sbroadcast st -> do
          modifyIORef' showInformations (st:)
        _ -> return ()

     where

       dd :: ([TClient],[TMonster]) ->TBody -> IO ([TClient],[TMonster])
       dd (as,bs) (TP c@(TClient n pos h b)) = if n == name
                                           then do
                                              writeIORef yourPositions pos
                                              writeIORef yourHealth h
                                              writeIORef yourBullets b
                                              return (as,bs)
                                           else return (c:as,bs)
       dd (as,bs) (TM m) = return $ (as,m:bs)



-- if read msg faid , then receive thread breakdown,
-- then sendInfomation thread breakdown.
-- then no one to takeMVar and keep it empty
-- then in main threaded ,when it happend input events
--an error happend __ maze-exe: thread blocked indefinitely in an MVar operation


    sendInformation name ClientState{..} = forever $ do
      opera <- takeMVar operateLis
      case opera of
        [] -> return ()
        os -> forM_ (reverse os) $ \o -> hPutStrLn connectServerHandler $ show $ Coperate o

