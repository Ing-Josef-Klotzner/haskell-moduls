module AufgageFFP3 where

import Data.List
import Control.Applicative

type Weight = Int
type Value = Int
type Item = (Weight, Value)
type Items = [Item]

type Load = [Item]

type Loads = [Load]
type LoadWghtVal = (Load, Weight, Value)

type MaxWeight = Weight

               
rs_generator :: Items -> Loads
rs_generator [] = []
rs_generator (i:is) =
    let
        prevSolution = rs_generator is
    in
      [i] : prevSolution ++ map (i:) prevSolution

rs_transformer :: Loads -> [LoadWghtVal]
rs_transformer loads =
    let
        sumPairs (a,b) (c,d) = (a+c, b+d)
        calcWeightVal load = foldr sumPairs (0, 0) load
        prependPair v (p1, p2) = (v, p1, p2)
    in
      zipWith prependPair loads . map calcWeightVal $ loads
    

rs_filter :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
rs_filter mw = filter (\(_, wa, _) -> wa <= mw)

rs_selector1 :: [LoadWghtVal] -> [LoadWghtVal]
rs_selector1 loads = filter (\(_,_,v) -> v == maxV) loads
    where
      maxV = maximum . map (\(_,_,v) -> v) $ loads

rs_selector2 ::  [LoadWghtVal] -> [LoadWghtVal]
rs_selector2 = rs_selector2' . rs_selector1

rs_selector2' :: [LoadWghtVal] -> [LoadWghtVal]
rs_selector2' [] = []
rs_selector2' loads = filter (\(_,w,_) -> w == minW) $ loads
    where
      minW = minimum . map (\(_,w,_) -> w) $  loads
             
-- binom:

binom :: (Integer,Integer) -> Integer
binom (n,k)
    | k==0 || n==k = 1
    | otherwise = binom (n-1,k-1) + binom (n-1,k)


binomM :: (Integer, Integer) -> Integer
binomM (n,k)
  | k == 0 || n == k = 1
  | k > n = 0
  | otherwise = binomTable !! fromInteger (n-1) !! fromInteger (k-1)
               + binomTable !! fromInteger (n-1) !! fromInteger k
                 
binomTable :: [[Integer]]
binomTable = map (\n -> map (curry binomM n) [0..n]) [0..]

{--
  n  k            : (n k)
  0  [0]          :       [1]
  1  [0,1]        :      [1,1]
  2  [0,1,2]      :     [1,2,1]
  3  [0,1,2,3]    :    [1,3,3,1]
  4  [0,1,2,3,4]  :   [1,4,6,4,1]
  5  [0,1,2,3,4,5]: [1,5,10,10,5,1]
--}
binomS :: (Integer, Integer) -> Integer
binomS (n,k) = binomSStream !! (fromInteger n) !! (fromInteger k)

binomSStream :: [[Integer]]
binomSStream = [1]:map (\l -> zipWith (+) (0:l) (l++[0])) binomSStream

        
check :: Bool
check = check1 && check2 && checkBinomM  && checkBinomS
      

check1 :: Bool
check1 = (rs_selector1 . rs_filter 5 . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)] == [([(2,7),(2,6)],4,13)]
         && (rs_selector1 . rs_filter 13 . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)] == [([(2,7),(10,100)],12,107)] 
         && (rs_selector1 . rs_filter 1 . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)] == []
         && (sort . rs_selector1 . rs_filter 5 . rs_transformer . rs_generator) [(5,13),(2,7),(2,6),(10,100)] == [([(2,7),(2,6)],4,13),([(5,13)],5,13)]


check2 :: Bool
check2 =(rs_selector2 . rs_filter 5 . rs_transformer . rs_generator) [(5,13),(2,7),(2,6),(10,100)] == [([(2,7),(2,6)],4,13)]

checkBinomM :: Bool
checkBinomM = binomM (20, 4) == binom (20, 4)
              && binomM (0, 0) == binom (0, 0) 
              && binomM (1, 1) == binom (1, 1)
              && binomM (21, 8) == binom (21, 8)
                 
checkBinomS :: Bool
checkBinomS = all (\nk -> binomS nk == binomM nk) [(1,1),(100,53),(300,57)]
