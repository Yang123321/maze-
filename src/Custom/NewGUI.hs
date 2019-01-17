{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Custom.NewGUI where


import SDL
import Linear (V4(..))
import Control.Monad (unless,forM_)
import Control.Concurrent
import qualified SDL.Font as F
import Data.Text (pack,unpack)
import SDL.Video.Renderer
import Data.Time
import Foreign.C.Types
import Data.IORef
import Data.List (foldl')
import qualified  Data.Text as T
import Data.IORef

import qualified Data.Map as M

import System.IO
import CreateMaze.Types
import Data.Binary
import Data.Array.Unboxed (UArray)
import Data.Array.IArray

import Network.Socket
import Custom.NewType
import Server.Types
import Custom.DealInput

import qualified Custom.NewClient as C
import Transmission.Command

import Control.Concurrent.Async

tmain :: IO ()
tmain = do

  nameMVar <- newEmptyMVar
  clientMVar <- newEmptyMVar
  errorRef <- newIORef " "

  async (C.startClient nameMVar clientMVar errorRef)

  initializeAll
  F.initialize

  window <- createWindow "great tower" $ defaultWindow { windowInitialSize = V2 620 620}
  renderer <- createRenderer window (-1) defaultRenderer

  let path = "data/font/OTF/s.otf"
  font <- F.load path 10

  tim <- getCurrentTime >>= newIORef

  appLoop tim 0 font renderer  (Logining "wellcome" nameMVar clientMVar errorRef)

  F.quit
  quit

appLoop :: IORef UTCTime
        -> Int
        -> F.Font
        ->  Renderer
        -> LoopState
        ->  IO ()
appLoop tim counter font renderer  loopstate = do
  t0 <- getCurrentTime
  writeIORef tim t0

  events <- pollEvents

  let eventIsQuit event = eventPayload event == QuitEvent
      qPressed = any eventIsQuit events

  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer

  case loopstate of
    Logining t nameMVar clientMVar errorRef -> do
      res <- tryTakeMVar clientMVar
      case res of
        Nothing -> do
          let (ls,backspace,enter) = foldl' inputEvent (t,0,False) events
              l = T.length ls
              ls' = T.take (l-backspace) ls

          drawText font renderer (0,400) "input your name: " counter
          drawText font renderer (100,400) ls' counter

          info <- readIORef errorRef
          drawText font renderer (100,430) (T.pack info) counter

          case enter of
            True -> do
              putMVar nameMVar (T.unpack ls')
              return ()
            False -> return ()

          present renderer
          delayFuction t0
          unless qPressed (appLoop tim (counter+1)  font renderer (Logining ls' nameMVar clientMVar errorRef))
        Just client -> do
          delayFuction t0
          unless qPressed (appLoop tim (counter+1)  font renderer (Playaing client (UserAction $ P $ V2 0 0)))

    Playaing client@ClientState{..} (TextInputing t) -> do
      let (ls,backspace,enter) = foldl' inputEvent (t,0,False) events
          l = T.length ls
          ls' = T.take (l-backspace) ls
      case enter of
        True -> do
          if T.length ls' == 0
            then do
            delayFuction t0
            unless qPressed (appLoop tim (counter+1)  font renderer (Playaing client (UserAction $ P $ V2 0 0)))
            else do
            putMVar operateLis $ [Command $ T.unpack ls']
            delayFuction t0
            unless qPressed (appLoop tim (counter+1)  font renderer (Playaing client (UserAction $ P $ V2 0 0)))
        False -> do

          renderMap renderer client
          renderOtherPlayers renderer font client

          renderMonsters renderer font client
          renderUser renderer font client

          ls <- readIORef $ showInformations
          renderInformation font renderer (0,400) ls counter

          drawText font renderer (0,300) ls' counter

          present renderer
          delayFuction t0
          unless qPressed (appLoop tim (counter+1)  font renderer (Playaing client (TextInputing ls')))


    Playaing client@ClientState{..} (UserAction p) -> do
      let (mousePos,operates,enter) = foldl' moveEvent (p,[],False) events
      heal <- readIORef yourHealth

      if heal <= 0
        then do 
        -- nameMVar <- newEmptyMVar
        -- clientMVar <- newEmptyMVar
        -- errorRef <- newIORef "your dead! "
        -- delayFuction t0
        -- appLoop tim counter font renderer  (Logining "wellcome" nameMVar clientMVar errorRef)
        return ()
        else do

        case enter of
          True -> do
            delayFuction t0
            unless qPressed (appLoop tim (counter+1)  font renderer (Playaing client (TextInputing T.empty)))
          False -> do
            putMVar operateLis operates
            renderMap renderer client
            renderOtherPlayers renderer font client

            renderMonsters renderer font client
            renderUser renderer font client
            modifyIORef' showInformations (take 20)
            ls <- readIORef $ showInformations
            renderInformation font renderer (0,400) ls counter

            present renderer
            delayFuction t0
            unless qPressed (appLoop tim (counter+1)  font renderer (Playaing client (UserAction $ mousePos)))


drawText font renderer (x,y) t n = do
  if T.length t == 0
    then do
    rendererDrawColor renderer $= black
    fillRect renderer (Just $ Rectangle  (P $ V2 (x+0+1) (y)) (V2 7 10))
    else do
    text <- F.blended font black $ t
    texture <- createTextureFromSurface renderer text
    freeSurface text
    TextureInfo _ _ w h <- queryTexture texture
    copy renderer texture Nothing (Just $ Rectangle (P $ V2 ( x :: CInt) ( y :: CInt)) (V2 (w :: CInt) (h :: CInt)))
    if n `mod` 14 == 0
      then do
      rendererDrawColor renderer $= black
      fillRect renderer (Just $ Rectangle  (P $ V2 (x+w+1) (y)) (V2 7 h))
      else return ()
    destroyTexture texture


delayFuction :: UTCTime -> IO ()
delayFuction t1 = do
  let delayTime60 = floor $ 1000 / 30
  t2 <- getCurrentTime
  let usedtim = floor $ 10^3 * diffUTCTime t2 t1
  threadDelay $ 1000 *  (delayTime60 - usedtim)


renderMap :: Renderer -> ClientState -> IO ()
renderMap renderer clientState@ClientState{..} = do
  (i,j) <- readIORef yourPositions
  forM_ (all i j ) $ \((x,y),v) -> do
    if v == 2
    then do
       rendererDrawColor renderer $= gray
       fillRect renderer (Just $ Rectangle  (P $ V2 (fromIntegral x*20) (fromIntegral y*20)) (V2 20 20))

    else do
       rendererDrawColor renderer $= V4 255 255 255 255
       fillRect renderer (Just $ Rectangle  (P $ V2 (fromIntegral x*20) (fromIntegral y*20)) (V2 20 20))

  where xy = [(x,y)| x <-[0..30],y<-[0..30]]
        vxy i j = map (\(x,y) -> baseMaze ! (i-15+y,j-15+x)) xy :: [Int]
        all i j = zip xy $ vxy i j


renderUser :: Renderer -> F.Font -> ClientState -> IO ()
renderUser renderer font clientState@ClientState{..} = do
  rendererDrawColor renderer $= green
  fillRect renderer (Just $ Rectangle  (P $ V2 (fromIntegral 15*20) (fromIntegral 15*20)) (V2 20 20))

-- draw blood bar
  h <- readIORef yourHealth
  rendererDrawColor renderer $= red
  drawRect renderer (Just $ Rectangle  (P $ V2 150 600) (V2 320 10))
  rendererDrawColor renderer $= red
  fillRect renderer (Just $ Rectangle  (P $ V2 150 600 ) (V2 (floor (fromIntegral h / 1000 * 320 )) 10))
    -- text <- F.blended font black $ pack $ (name++" ")
    -- texture <- createTextureFromSurface renderer text
    -- freeSurface text


  (i0,j0) <- readIORef yourPositions

-- draw bullets
  rendererDrawColor renderer $= green
  bulltes <- readIORef yourBullets
  forM_ bulltes $ \(i,j) -> do
    let (x,y) = (( (15 - (fromIntegral j0-j))*20 ),( (15 - (fromIntegral i0-i))*20))
    fillRect renderer (Just $ Rectangle  (P $ V2 (floor x) (floor y)) (V2 5 5))




renderOtherPlayers :: Renderer -> F.Font -> ClientState -> IO ()
renderOtherPlayers renderer font clientState@ClientState{..} = do
  (i0,j0) <- readIORef yourPositions
  otherPlayers <- readIORef playersPosition
  let ls =filter (\(TClient _ (i,j) _ _) -> abs (i-i0) < 16 && abs (j-j0) < 16 )  otherPlayers

  forM_ ls $ \(TClient name (i,j) h bs) -> do
    rendererDrawColor renderer $= blue
    let (x,y) = ((fromIntegral (15 - (j0-j))*20),(fromIntegral (15 - (i0-i))*20))
    fillRect renderer (Just $ Rectangle  (P $ V2 x y ) (V2 20 20))

  --draw bullets
    rendererDrawColor renderer $= red
    forM_ bs $ \(i,j) -> do
      let (x,y) = (( (15 - (fromIntegral j0-j))*20 ),( (15 - (fromIntegral i0-i))*20))
      fillRect renderer (Just $ Rectangle  (P $ V2 (floor x) (floor y)) (V2 5 5))


-- draw blood bar
    rendererDrawColor renderer $= red
    drawRect renderer (Just $ Rectangle  (P $ V2 (x-15) (y+22) ) (V2 50 5))
    fillRect renderer (Just $ Rectangle  (P $ V2 (x-15) (y+22) ) (V2 (floor (fromIntegral h / 1000 * 50 )) 5))
--draw name
    text <- F.blended font black $ pack  name
    texture <- createTextureFromSurface renderer text
    freeSurface text

    TextureInfo _ _ w h <- queryTexture texture
    copy renderer texture Nothing (Just $ Rectangle (P $ V2 (x+10-div w 2) (y-h)) (V2 (w :: CInt) (h :: CInt)))
    destroyTexture texture

-- renderBlood :: Renderer -> Int -> IO ()

renderMonsters :: Renderer -> F.Font -> ClientState -> IO ()
renderMonsters renderer font clientState@ClientState{..} = do
  (i0,j0) <- readIORef yourPositions
  monsterList <- readIORef monsterList
  let ls =filter (\(TMonster _ _ (i,j) _) -> abs (i-i0) < 16 && abs (j-j0) < 16 )  monsterList


  rendererDrawColor renderer $= red -- V4 255 255 255 100
  forM_ ls $ \(TMonster name health (i,j) pointList) -> do
    -- rect
    let (x,y) = ((fromIntegral (15 - (j0-j))*20),(fromIntegral (15 - (i0-i))*20))
    fillRect renderer (Just $ Rectangle  (P $ V2 x y ) (V2 20 20))

    -- blood bar
    rendererDrawColor renderer $= red
    drawRect renderer (Just $ Rectangle  (P $ V2 (x-15) (y+22) ) (V2 50 5))
    fillRect renderer (Just $ Rectangle  (P $ V2 (x-15) (y+22) ) (V2 (floor (fromIntegral health / 1000 * 50 )) 5))

    -- name
    text <- F.blended font black $ pack  name
    texture <- createTextureFromSurface renderer text
    freeSurface text

    TextureInfo _ _ w h <- queryTexture texture
    copy renderer texture Nothing (Just $ Rectangle (P $ V2 (x+10-div w 2) (y-h)) (V2 (w :: CInt) (h :: CInt)))
    destroyTexture texture


    -- bullets
    forM_ pointList $ \(i,j) -> do
      let (x,y) = (( (15 - (fromIntegral j0-j))*20 ),( (15 - (fromIntegral i0-i))*20))
      fillRect renderer (Just $ Rectangle  (P $ V2 (floor x) (floor y)) (V2 3 3))





renderInformation :: F.Font ->Renderer -> (CInt , CInt )-> [String] -> Int ->   IO ()
renderInformation font renderer (x0,y0) ls counter= do
  let l = length ls
      ls' = if l > 20 then ls else ls

  rendererDrawColor renderer $= V4 255 255 255 100
  forM_ (zip [0..] ls') $ \(y,st) -> do
    text <- F.blended font black $ pack $ (st++" ")
    texture <- createTextureFromSurface renderer text
    freeSurface text
    TextureInfo _ _ w h <- queryTexture texture
    copy renderer texture Nothing (Just $ Rectangle (P $ V2 ( x0 :: CInt) ( y0 + y * 10 :: CInt)) (V2 (w :: CInt) (h :: CInt)))
    destroyTexture texture


red :: F.Color
red = V4 255 0 0 255

black :: F.Color
black =V4 0 0 0 255

green :: F.Color
green = V4 0 255 0 255

blue :: F.Color
blue = V4 0 0 255 255


gray :: F.Color
gray = V4 128 128 128 255
