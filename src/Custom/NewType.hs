module Custom.NewType where

import Data.IORef
import qualified Data.Map as M
import System.IO
import Data.Array.Unboxed (UArray)
import Transmission.Command
import Control.Concurrent.MVar
import Server.Types
import Linear.V2 (V2)
import SDL (Point)
import Data.Int (Int32)
import Data.Text

data ClientState
  =ClientState { yourName :: String
               , yourPositions :: (IORef (Int,Int))
               , yourHealth :: IORef Int

               , yourBullets :: IORef [(Float,Float)]
               , playersPosition ::  (IORef [TClient]) -- other players

               , monsterList :: (IORef [TMonster])

               , baseMaze :: (UArray (Int,Int) Int) -- maze
               , showInformations :: (IORef [String])
               , connectServerHandler :: Handle
               , operateLis :: MVar [Operate]
               }


data LoopState =  Logining Text (MVar String) (MVar ClientState) (IORef String)
               | Playaing ClientState UserDoing

data UserDoing = TextInputing Text -- user input massge to server
               | UserAction (Point V2 Int32)  -- user action : Move , shot bullets. need to record mouse position
