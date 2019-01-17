module CreateMaze.Render(write) where

import Codec.Picture (Pixel8,Image,withImage,writeBitmap)
import CreateMaze.Types
import Control.Monad.ST (ST,runST)
import Data.Array.ST
import Data.Array.MArray
import Data.Array.IArray (Array,bounds,(!))

import Data.Array.Unboxed (UArray)


import System.Random
import Data.Time



import Data.Word (Word8)




render :: MazeI -> ST s (Image Pixel8)
render maze = do
  let ((a,b),(c,d)) = bounds maze
  let (w,h) = (c-a+1,d-b+1)
  withImage w h (\i j -> return $ state2pixel $  maze ! (j,i))



state2pixel :: Int -> Pixel8
state2pixel number | number == 0 = 200  -- Null
                   | number <0 = 100 -- ConnectPoint
                   | number < maxTest = 0 -- Room
                   | otherwise = 50 --Floor



write :: String -> MazeI -> IO ()
write st  maze = do
  let image = runST $ render maze
  writeBitmap st image


