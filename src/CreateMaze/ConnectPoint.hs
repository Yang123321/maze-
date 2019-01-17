module CreateMaze.ConnectPoint where



import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array

import CreateMaze.Render
import CreateMaze.Types

import System.Random
import Control.Monad

import Data.STRef.Strict
import qualified Data.Map as M
import qualified Data.Set as S


markConnectPoint :: Maze s -> (Int,Int) -> ST s ()
markConnectPoint maze (mw,mh) = do
  let allPos = [(i,j) | i <- [1..mw-2],j <- [1..mh-2]]
  forM_ allPos $ \p -> do
    b <- check maze p
    if b
      then writeArray maze p (-1)
      else return ()

  where check :: Maze s -> (Int,Int) -> ST s Bool
        check m (i,j) = do  let ps1 = [(i-1,j),(i,j),(i+1,j)]
                                ps2 = [(i,j-1),(i,j),(i,j+1)]
                            vs1 <- forM ps1 $ readArray m
                            vs2 <- forM ps2 $ readArray m
                            return $ checks vs1 || checks vs2

        checks [a,0,c] | a > 0 && a < maxTest && c > maxTest = True
                       | a > maxTest && c > 0  && c <maxTest  = True
                       | a > 0 && a < maxTest && c >0 && c <maxTest  = True
                       | otherwise = False
        checks _ = False




markConnectPointWithRoomMap :: Maze s -> (Int,Int) ->RoomMap ->  ST s RoomMap
markConnectPointWithRoomMap maze (mw,mh) rmM = do
  roomMap <- newSTRef rmM

  let allPos = [(i,j) | i <- [1..mw-2],j <- [1..mh-2]]
  forM_ allPos $ \p@(i,j) -> do
    b <- check maze p
    if b
      then do
      writeArray maze p (-1)
      ls <- forM [(i+1,j),(i-1,j),(i,j+1),(i,j-1)] $ readArray maze
      let ls' = filter (\t -> t/=0 && t/= (-1)) ls
      forM_ ls' $ \k -> modifySTRef' roomMap $ M.insertWith add k (Floor (S.singleton p))

      else return ()
  readSTRef roomMap

  where check :: Maze s -> (Int,Int) -> ST s Bool
        check m (i,j) = do  let ps1 = [(i-1,j),(i,j),(i+1,j)]
                                ps2 = [(i,j-1),(i,j),(i,j+1)]
                            vs1 <- forM ps1 $ readArray m
                            vs2 <- forM ps2 $ readArray m
                            return $ checks vs1 || checks vs2

        checks [a,0,c] | a > 0 && a < maxTest && c > maxTest = True
                       | a > maxTest && c > 0  && c <maxTest  = True
                       | a > 0 && a < maxTest && c >0 && c <maxTest  = True
                       | otherwise = False
        checks _ = False

        add :: RoomAndFloor -> RoomAndFloor -> RoomAndFloor
        add (Floor p) (Room a b) = Room a (p `S.union`b)
        add (Floor p) (Floor b) = Floor (p `S.union` b)
        add _ _ = error "erro at CreateMaze.ConnectPoint.hs 58 line"
