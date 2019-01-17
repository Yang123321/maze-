module CreateMaze.RandomList where



import System.Random
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array.IArray
import Control.Monad


randomArray :: StdGen -> Int  -> Array Int Int
randomArray gen l = runSTArray $ do
  arr <- newListArray (0,l-1) [0..l-1]
  forM_ ls $ swap arr
  return arr
  where ls = take l $ sp $ randomRs (0,l-1) gen
        sp (m:n:ns) = (m,n) : sp ns

        swap :: STArray s Int Int -> (Int,Int) -> ST s ()
        swap arr (i,j)= do
          a <- readArray arr i
          b <- readArray arr j
          writeArray arr i b
          writeArray arr j a

randomList :: StdGen -> Int -> [a] -> [a]
randomList gen l ls = map (ls !! ) arr
  where arr = elems $ randomArray gen l


test = randomList (mkStdGen 10) 9 [1..9]


