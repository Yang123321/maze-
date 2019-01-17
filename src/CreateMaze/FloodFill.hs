module CreateMaze.FloodFill where

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

import CreateMaze.RandomList (randomList)

fillAllWithRoomMap :: Maze s -> (Int,Int) -> RoomMap -> ST s RoomMap
fillAllWithRoomMap maze (mw,mh) rmM= do
  number <- newSTRef (maxTest + 1)
  roomMap <- newSTRef rmM

  let allPos = [(i,j) | i <- [1..mw-2],j <- [1..mh-2]]
  forM_ allPos $ \p -> do
    b <- check maze p
    if b
      then do
      number' <- readSTRef number
      floodfill maze (mw,mh) number' p

      modifySTRef' roomMap (M.insert number' (Floor S.empty) )

      modifySTRef' number (+1)
      else return ()
  readSTRef roomMap

  where check :: Maze s -> (Int,Int) -> ST s Bool
        check m (i,j) = do  let ps = [(i,j),(i+1,j),(i-1,j), (i,j+1),(i+1,j+1),(i-1,j+1),(i,j-1),(i+1,j-1),(i-1,j-1)]
                            vs <- forM ps $ readArray m
                            return $ all (==0) vs


fillAllWithRoomMapRandom :: StdGen -> Maze s -> (Int,Int) -> RoomMap -> ST s RoomMap
fillAllWithRoomMapRandom gen maze (mw,mh) rmM= do
  number <- newSTRef (maxTest + 1)
  roomMap <- newSTRef rmM

  let allPos = [(i,j) | i <- [1..mw-2],j <- [1..mh-2]]
  forM_ allPos $ \p -> do
    b <- check maze p
    if b
      then do
      number' <- readSTRef number
      floodfill' gen maze (mw,mh) number' p

      modifySTRef' roomMap (M.insert number' (Floor S.empty) )

      modifySTRef' number (+1)
      else return ()
  readSTRef roomMap

  where check :: Maze s -> (Int,Int) -> ST s Bool
        check m (i,j) = do  let ps = [(i,j),(i+1,j),(i-1,j), (i,j+1),(i+1,j+1),(i-1,j+1),(i,j-1),(i+1,j-1),(i-1,j-1)]
                            vs <- forM ps $ readArray m
                            return $ all (==0) vs




fillAll :: Maze s -> (Int,Int) -> ST s ()
fillAll maze (mw,mh) = do
  number <- newSTRef (maxTest + 1)
  let allPos = [(i,j) | i <- [1..mw-2],j <- [1..mh-2]]
  forM_ allPos $ \p -> do
    b <- check maze p
    if b
      then do
      number' <- readSTRef number
      floodfill maze (mw,mh) number' p

      modifySTRef' number (+1)
      else return ()

  where check :: Maze s -> (Int,Int) -> ST s Bool
        check m (i,j) = do  let ps = [(i,j),(i+1,j),(i-1,j), (i,j+1),(i+1,j+1),(i-1,j+1),(i,j-1),(i+1,j-1),(i-1,j-1)]
                            vs <- forM ps $ readArray m
                            return $ all (==0) vs

floodfill ::Maze s -> (Int,Int) -> Int -> (Int,Int) -> ST s ()
floodfill maze (mw,mh) number pos@(i',j')= do
  if i' >0 && i' < mw-1 &&  j'>0 && j' < mh-1
    then do
    v <- readArray maze pos
    if v == 0 -- the position is Null (word8 ==0)
      then do

      -- direct right
      let directRiht i j = [ (i+1,j),(i+1,j+1),(i+1,j-1)]
      vsr <- forM (directRiht i' j') $ readArray maze
      if all (== 0) vsr -- direct right are all Null
        then
        if i' == mw -2
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsr1 <- forM (directRiht (i'+1) j') $ readArray maze
          if all (==0) vsr1
            then do
            floodfill maze (mw,mh) number (i'+1,j')
            else
            return ()
          return ()

        else return ()



      -- direct down
      let directDown i j = [ (i,j+1),(i+1,j+1),(i-1,j+1)]
      vsd <- forM (directDown i' j') $ readArray maze
      if all (== 0) vsd -- direct right are all Null
        then
        if j' == mh -2
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsd1 <- forM (directDown i' (j'+1)) $ readArray maze
          if all (==0) vsd1
            then do
            floodfill maze (mw,mh) number (i',j'+1)
            else
            return ()
          return ()

        else return ()




      -- direct left
      let directLeft i j = [ (i-1,j),(i-1,j+1),(i-1,j-1)]
      vsl <- forM (directLeft i' j') $ readArray maze
      if all (== 0) vsl -- direct right are all Null
        then
        if i' == 1
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsl1 <- forM (directLeft (i'-1) j') $ readArray maze
          if all (==0) vsl1
            then do
            floodfill maze (mw,mh) number (i'-1,j')
            else
            return ()
          return ()

        else return ()






      -- direct up
      let directUp i j = [ (i,j-1),(i+1,j-1),(i-1,j-1)]
      vsu <- forM (directUp i' j') $ readArray maze
      if all (== 0) vsu -- direct right are all Null
        then
        if j' == 1
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsu1 <- forM (directUp i' (j'-1)) $ readArray maze
          if all (==0) vsu1
            then do
            floodfill maze (mw,mh) number (i',j'-1)
            else
            return ()
          return ()

        else return ()




      else return ()

    else return ()





floodfill' ::StdGen -> Maze s -> (Int,Int) -> Int -> (Int,Int) -> ST s ()
floodfill' gen maze (mw,mh) number pos@(i',j')= do
  if i' >0 && i' < mw-1 &&  j'>0 && j' < mh-1
    then do
    let (gen1,gen2) = split gen
        (gen3,gen4) = split gen1
        (g1,g2) = split gen3
        (g3,g4) = split gen4
    v <- readArray maze pos
    if v == 0 -- the position is Null (word8 ==0)
      then sequence_ $ randomList gen2 4 [setp1 g1,setp2 g2,setp3 g3,setp4 g4]

      else return ()
    else return ()

    where
     setp1 g= do
      -- direct right
      let directRiht i j = [ (i+1,j),(i+1,j+1),(i+1,j-1)]
      vsr <- forM (directRiht i' j') $ readArray maze
      if all (== 0) vsr -- direct right are all Null
        then
        if i' == mw -2
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsr1 <- forM (directRiht (i'+1) j') $ readArray maze
          if all (==0) vsr1
            then do
            -- return ()
            floodfill' g maze (mw,mh) number (i'+1,j')
            else
            return ()
          return ()

        else return ()



     setp2 g= do
      -- direct down
      let directDown i j = [ (i,j+1),(i+1,j+1),(i-1,j+1)]
      vsd <- forM (directDown i' j') $ readArray maze
      if all (== 0) vsd -- direct right are all Null
        then
        if j' == mh -2
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsd1 <- forM (directDown i' (j'+1)) $ readArray maze
          if all (==0) vsd1
            then do
            -- return ()
            floodfill' g maze (mw,mh) number (i',j'+1)
            else
            return ()
          return ()

        else return ()




     setp3 g= do
      -- direct left
      let directLeft i j = [ (i-1,j),(i-1,j+1),(i-1,j-1)]
      vsl <- forM (directLeft i' j') $ readArray maze
      if all (== 0) vsl -- direct right are all Null
        then
        if i' == 1
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsl1 <- forM (directLeft (i'-1) j') $ readArray maze
          if all (==0) vsl1
            then do
            -- return ()
            floodfill' g maze (mw,mh) number (i'-1,j')
            else
            return ()
          return ()

        else return ()






     setp4 g= do
      -- direct up
      let directUp i j = [ (i,j-1),(i+1,j-1),(i-1,j-1)]
      vsu <- forM (directUp i' j') $ readArray maze
      if all (== 0) vsu -- direct right are all Null
        then
        if j' == 1
          then do
          writeArray maze pos number
          else do
          writeArray maze pos number
          vsu1 <- forM (directUp i' (j'-1)) $ readArray maze
          if all (==0) vsu1
            then do
            -- return ()
            floodfill' g maze (mw,mh) number (i',j'-1)
            else
            return ()
          return ()

        else return ()


































