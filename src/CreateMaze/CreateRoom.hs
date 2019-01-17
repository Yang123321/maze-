module CreateMaze.CreateRoom where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array.IArray

import CreateMaze.Render
import CreateMaze.Types
import System.Random
import Control.Monad

import CreateMaze.FloodFill (floodfill,fillAll,fillAllWithRoomMap,fillAllWithRoomMapRandom)
import CreateMaze.ConnectPoint (markConnectPoint,markConnectPointWithRoomMap)
import CreateMaze.ConnectRoomAndFloor (assimilation')
import CreateMaze.ReverseCarve (reverseCarveAll)

import Data.STRef.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.List
import Data.Binary

initMaze :: ST s (Maze s)
initMaze = do
  newArray ((0,0),(mwDefault-1,mhDefault-1)) 0



randomRoomScaleList :: StdGen -> [(Int,Int,Int,Int)] -- [(i,j,rw,rh)]
randomRoomScaleList gen = zipWith (\ (a,b) (c,d) -> (a,b,c,d)) posList scaleList
  where lw = bounds rwList
        lh = bounds rhList
        (g1,g2) = split gen

-----------------------------------
        (g3,g4) = split g1
        l1 = randomRs lw g3
        l2 = randomRs lh g4
        scaleList = zipWith (\i j -> (rwList ! i ,rhList !j)) l1 l2
---------------------------------------
        genList = map mkStdGen (randoms g2 :: [Int])
        posList = zipWith randomPos genList scaleList


randomPos :: StdGen -> (Int,Int) -> (Int,Int)
randomPos gen (rw,rh) = (i,j)
  where (g1,g2) = split gen
        i =head $ filter odd $ randomRs (1,mwDefault-rw-1) g1
        j =head $ filter odd $ randomRs (1,mhDefault-rh-1) g2


carveMaze :: StdGen -> Maze s -> Int -> ST s ()
carveMaze gen maze n = do
  let alls = map (\ (a,b,c,d)->(a,b,a+c-1,b+d-1)) $ take n $ randomRoomScaleList gen
  forM_ alls $ \(i,j,i',j') -> do
    let ps = [(a,b) | a <- [i..i'],b<-[j..j']]
    psv <- forM ps $ readArray maze
    if all (==0) psv -- all the psv's position is Null (word8 ==0)
      then forM_ ps $ \p -> writeArray maze p 2
      else return ()

carveMazeWithRoomMap :: StdGen -> Maze s -> Int -> ST s RoomMap
carveMazeWithRoomMap gen maze n = do
  number <- newSTRef (1 ::Int)
  roomMap <- newSTRef (M.empty :: RoomMap)

  let alls = map (\ (a,b,c,d)->(a,b,a+c-1,b+d-1)) $ take n $ randomRoomScaleList gen
  forM_ alls $ \info@(i,j,i',j') -> do
    let ps = [(a,b) | a <- [i..i'],b<-[j..j']]
    psv <- forM ps $ readArray maze
    if all (==0) psv -- all the psv's position is Null (word8 ==0)
      then do
      number' <- readSTRef number

      forM_ ps $ \p -> writeArray maze p number'

      modifySTRef' roomMap (M.insert number' (Room info S.empty) )

      modifySTRef' number (+1)

      else return ()
  readSTRef roomMap




createMaze n = do
  fileNumber <- randomIO :: IO Int
  let [g1,g2,g3] = take 3 $ map mkStdGen $ randoms (mkStdGen fileNumber) 
      (nmaze,roomMap2) = runST $ do
            maze <- initMaze
            roomMap <- carveMazeWithRoomMap g1 maze $ min maxTest n
            roomMap1 <-fillAllWithRoomMapRandom g2 maze (mwDefault,mhDefault) roomMap
            roomMap2 <-  markConnectPointWithRoomMap maze (mwDefault,mhDefault) roomMap1
            s <-assimilation' g3 maze 2 roomMap2 

            reverseCarveAll maze (mwDefault,mhDefault) 2
            nmaze <- freeze maze
            return (nmaze,roomMap2)

      roomList = M.foldlWithKey' communicata [] roomMap2
      roomList' =concat $ intersperse "\n" $ map show $ map (\(i,j) -> (i,change j)) roomList
      filePath1 = concat [ "data/test/random/",show fileNumber,".txt"]
      filePath2 = concat [ "data/test/random/",show fileNumber,".bmp"]
      filePath3 = concat [ "data/test/random/",show fileNumber,".mazeI"]
      filePath4 = concat [ "data/test/random/",show fileNumber,".rooMap"]


  writeFile filePath1 roomList'
  write filePath2 nmaze
  encodeFile filePath3 nmaze
  encodeFile filePath4 roomMap2

  where communicata :: [(Int,RoomAndFloor)] -> Int -> RoomAndFloor -> [(Int,RoomAndFloor)]
        communicata b k c@(Room m n) = (k,c):b
        communicata b k _ = b

        change :: RoomAndFloor -> (Int,Int,Int,Int)
        change (Room a b) = a
        change _ = error "nice"


