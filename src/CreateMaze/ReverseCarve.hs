module CreateMaze.ReverseCarve where 



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





reverseCarveAll :: Maze s -> (Int,Int) -> Int ->  ST s ()
reverseCarveAll maze (mw,mh) v = do
  let allPos = [(i,j) | i <- [1..mw-2],j <- [1..mh-2]]
  forM_ allPos $ \p -> do
    reverseCarve maze v p




reverseCarve :: Maze s -> Int -> (Int,Int) -> ST s ()
reverseCarve maze v  p@(i,j)  = do
  v' <- readArray maze p
  if v' /= v
    then return ()
    else do
    vs <- forM [(i+1,j),(i-1,j),(i,j+1),(i,j-1)] $ readArray maze
    let vs' = filter (==0) vs
    if length vs' == 3
      then do
      let [p'] =  map snd $ filter ((==v).fst) $ zip vs [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]
      writeArray maze p 0
      reverseCarve maze v p'
      else return ()
    return ()

