{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module Custom.DealInput where

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
import qualified Data.Text as T

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

import qualified Custom.NewClient as C
import Transmission.Command

import Control.Concurrent.Async
import qualified Transmission.TestPingClient as T



eventIsQuit event = eventPayload event == QuitEvent

qPressed events = any eventIsQuit events


moveEvent (mousePosition, ops,enter) event
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeRight
    || keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeD = (mousePosition,Oright : ops,enter)
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeDown
    || keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeS = (mousePosition,Odown : ops,enter)
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeLeft
    || keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeA =(mousePosition, Oleft : ops,enter)
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeUp
    || keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeW = (mousePosition,Oup : ops,enter)
--- enter pressed
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == (Keycode 13) = (mousePosition,ops,True)

---------------------------bullet speed up
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeE = (mousePosition,(BulletSpeedUp 1) : ops,enter)

  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ = (mousePosition,(BulletSpeedUp (-1)) : ops,enter)

----------------shot event, space pressed,get mouse position , calculate to direct
  | KeyboardEvent keyboardEvent <- eventPayload event
  , keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeSpace = (mousePosition, (Shot (calculateDir mousePosition)) : ops,enter)

  | MouseMotionEvent (MouseMotionEventData _ _ _ p _) <- eventPayload event = (p,ops,enter)
  | MouseButtonEvent (MouseButtonEventData _ Pressed _ ButtonLeft _ p) <- eventPayload event = (p,(Shot (calculateDir p)) : ops,enter)
moveEvent o _ = o



inputEvent (ls,backspace,enter) event
  | KeyboardEvent keyboardEvent <- eventPayload event
  ,keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == (Keycode 13) = (ls,backspace,True)
  | KeyboardEvent keyboardEvent <- eventPayload event
  ,keyboardEventKeyMotion keyboardEvent == Pressed
  , keysymKeycode (keyboardEventKeysym keyboardEvent) == (KeycodeBackspace) = (ls,backspace+1,enter)
  | TextInputEvent (TextInputEventData _ t) <- eventPayload event = (ls `T.append` t,backspace,enter)
inputEvent (ls,backspace,enter) _  = (ls,backspace,enter)

calculateDir (P (V2 x y)) =
        let (x',y') = (fromIntegral x-310,310- fromIntegral y)
            distance = sqrt $ x'^2 + y'^2
        in (x' / distance, y' / distance)

