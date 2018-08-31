module Shuffle (shuffleIO, shuffle) where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Foldable (toList)
import Data.Set.Ordered
import Data.Tuple (swap)

-- Source : https://wiki.haskell.org/Random_shuffle 
 
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffleList :: [a] -> StdGen -> ([a],StdGen)
shuffleList xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n =  newListArray (1,n)

shuffleListIO :: [a] -> IO [a]
shuffleListIO xs = getStdRandom (shuffleList xs)

shuffleIO :: Ord a => OSet a -> IO (OSet a)
shuffleIO xs = fmap fromList $ getStdRandom (shuffleList $ toList xs)

shuffle :: Ord a => OSet a -> StdGen -> (StdGen, OSet a)
shuffle xs stdGen = fmap fromList $ swap $ shuffleList (toList xs) stdGen
