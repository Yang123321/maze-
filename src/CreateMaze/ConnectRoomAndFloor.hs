module CreateMaze.ConnectRoomAndFloor where



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

type StartRoomNumber = Int


assimilation' :: StdGen -> Maze s -> StartRoomNumber -> RoomMap -> ST s ()
assimilation' gen maze startNumber roomMap =
  case M.lookup startNumber roomMap of
    Nothing -> error "error at ConnectRoomAndFloor.hs -- function  assimilation'"
    Just (Room rbounds positionSet) -> do 
      let roomMap' = M.delete startNumber roomMap
      loop' gen maze startNumber roomMap' positionSet


loop' :: StdGen -> Maze s -> StartRoomNumber -> RoomMap -> S.Set (Int,Int) -> ST s ()
loop' gen maze startNumber roomMap positionSet  =
  if M.size roomMap == 0 || S.size positionSet == 0
  then return ()
  else do
    let (g1,g2) = split gen
    let connectPoint@(i,j) = head $ S.toList positionSet --- need random
        boolList = map (\n -> n `elem` [5]) (randomRs (1,10) g1 :: [Int])

    vs <- forM [(i+1,j),(i-1,j),(i,j+1),(i,j-1)] $ readArray maze

    let [v] = filter (\x -> x /=0 && x /= (-1) && x /= startNumber) vs -- shold only one value

    case M.lookup v roomMap of
      Nothing -> error "error at ConnectRoomAndFloor.hs -- function loop'"

      Just (Room (a,b,a',b') positionSet') -> do
        let dif = S.difference positionSet $ S.difference positionSet positionSet'
            uni = S.union positionSet positionSet' `S.difference` dif

        forM_ (zip boolList $ S.toList dif) $ \(bool,p) -> do
          if bool
            then writeArray maze p startNumber 
            else writeArray maze p 0
        writeArray maze connectPoint startNumber -- assimilation a connectPoint
        forM_ [(i,j) | i <- [a..a'],j<-[b..b']] $ \p -> writeArray maze p startNumber
        let roomMap' = M.delete v roomMap
        loop' g2 maze startNumber roomMap' uni

      Just (Floor positionSet') -> do
        let dif = S.difference positionSet $ S.difference positionSet positionSet'
            uni = S.union positionSet positionSet' `S.difference` dif

        forM_ (zip boolList $ S.toList dif) $ \(bool,p) -> do
          if bool
            then writeArray maze p startNumber 
            else writeArray maze p 0
        writeArray maze connectPoint startNumber -- assimilation a connectPoint
        search maze v (i,j)
        let roomMap' = M.delete v roomMap
        loop' g2 maze startNumber roomMap' uni

          where search :: Maze s -> Int -> (Int,Int) -> ST s ()
                search maze v (i,j)  = do
                  vs <- forM [(i+1,j),(i-1,j),(i,j+1),(i,j-1)] $ readArray maze
                  let vs' =  map snd $ filter ((==v).fst) $ zip vs [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]
                  forM_ vs' $ \p' -> do
                    writeArray maze p' startNumber
                    search maze v p'


