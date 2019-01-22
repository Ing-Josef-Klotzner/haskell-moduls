{-# LANGUAGE TupleSections #-}
{-# OPTIONS -O2 -optc-O2 -funbox-strict-fields #-}


import Data.List
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.State
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map

-- Reading

readInt :: State [L.ByteString] Int
readInt = do
  x:xs <- get
  put xs
  return $ read $ L.unpack x

readIntTuple2 :: State [L.ByteString] (Int, Int)
readIntTuple2 = do
  a <- readInt
  b <- readInt
  return (a, b)

readIntTuple3 :: State [L.ByteString] (Int, Int, Int)
readIntTuple3 = do
  a <- readInt
  b <- readInt
  c <- readInt
  return (a, b, c)

readArray :: Int -> State [L.ByteString] a -> State [L.ByteString] (V.Vector a)
readArray n r = V.generateM n (\_ -> r)

readIntArray :: Int -> State [L.ByteString] (V.Vector Int)
readIntArray n = readArray n readInt

-- 

main :: IO ()
main = do
  contents <- L.words <$> L.getContents
  ((n, m, r, a), _) <- return $ flip runState contents $ do
    n <- readInt
    m <- readInt
    r <- readInt
    a <- readArray n (readArray m readInt)
    return (n, m, r, a)
  forM_ [1..n] $ \i -> do
    forM_ [1..m] $ \j -> do
      let k = minimum [i-1, n-i, j-1, m-j]
      if (k < n-k) && (k < m-k)
        then do
          let l = reverse $ ((,k+1) <$> [k+1..n-k]) ++ ((n-k,) <$> [k+2..m-k]) ++ ((,m-k) <$> reverse [k+1..n-k-1]) ++ ((k+1,) <$> reverse [k+2..m-k-1])
          let ind = r + (fromJust . findIndex (== (i,j)) $ l)
          let (i', j') = l !! (ind `mod` length l)
          putStr $ show (a V.! (i'-1) V.! (j'-1)) ++ " "
        else putStr $ show (a V.! (i-1) V.! (j-1)) ++ " "
    putChar '\n'
