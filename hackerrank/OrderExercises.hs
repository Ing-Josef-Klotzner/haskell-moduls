module Main where
import Control.Monad (liftM)
import Data.List (sort)
--import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

readLst :: IO [Int]
readLst = liftM (map read . words) getLine

-- find biggest sum of slice ("subarray") - slow O (n²)
-- for slice length from 1 to length of inputV
--      from startindex 0 to length of inputV (minus slice length)
--      return max sum of slice and related startindex and slice length
findBgst :: V.Vector Int -> (Int, Int, Int)
findBgst inputV = go 0 0 0 0 1 where
    go sm sim lgm si lg
        | si == 0 && lg == len = if smN > sm then (smN, si, lg) else (sm, sim, lgm)
        | smN > sm = go smN si lg siI lgI
        | True = go sm sim lgm siI lgI
        where
        smN = V.sum $ V.slice si lg inputV
        siI = if si + lg < len then si + 1 else 0
        lgI = if siI == 0 && lg < len then lg + 1 else lg 
        len = V.length inputV

-- faster: Kadane algorithm O (n)
-- find maximum sum of subarray, return (sum, startindex, length)
kadane :: V.Vector Int -> (Int, Int, Int)
kadane inputV = go 0 sm1 sm1 0 0 1 where
    sm1 = inputV V.! 0
    go simOld sm meh sim lgm si
        | si == len = (sm, simOld, lgm)
        | True = go simON smN mehN simN lgmN (si + 1)
        where
        simON = if newMax then sim else simOld
        simN = if newStart then si + 1 else sim
        lgmN = if newMax then si - simON + 1 else lgm
        newStart = mehN < 0
        newMax = smN /= sm
        currNr = inputV V.! si
        smN = max sm mehN
        mehN = max currNr (meh + currNr)
        len = V.length inputV
-- faster in total - create a reverse sorted list of maxima during kadane search - O (n * log n)
kadaneL :: V.Vector Int -> Int -> [Int]
kadaneL inputV k = go sm1 sm1 1 [] where
    sm1 = inputV V.! 0
    go sm meh si kadL
        | si == len = take k $ reverse $ sort kadLl
        | True = go smN mehNN (si + 1) kadLN
        where
        kadLl = if sm > 0 then kadL ++ [sm] else kadL
        kadLN = if newStart && sm > 0 then kadL ++ [sm] else kadL
        -- max until next newStartg (mehN < 0) - is there a bigger number behind?
        -- done with a sub kadane search
        maxNnewStart = gog 0 meh (si - 0)
            where
            gog smg mehg sig
                | sig == len || newStartg = smg -- error (show smg ++ " " ++ show sig) --smg
                | True = gog smgN mehgN (sig + 1)
                where
                currNrg = inputV V.! sig
                newStartg = mehgN < 0
                smgN = max smg mehgN
                mehgN = max currNrg (mehg + currNrg)
        newStart = mehN < 0 || newDsjnt
        newDsjnt = if mehN < meh && mehN > 0 then maxNnewStart < meh else False
--        newMax = smN > sm
        currNr = inputV V.! si
        smN = if newStart then 0 else max sm mehN
        mehNN = if newDsjnt then 0 else mehN
        mehN = max currNr (meh + currNr)
        len = V.length inputV
{-def max_subarray(A):
    max_ending_here = max_so_far = A[0]                 meh
    for x in A[1:]:
        max_ending_here = max(x, max_ending_here + x)
        max_so_far = max(max_so_far, max_ending_here)   sm
    return max_so_far -}
shorten :: V.Vector Int -> Int -> Int -> V.Vector Int
shorten inputV si lg = (V.slice 0 si inputV) V.++ (V.slice (si + lg) (V.length inputV - si - lg) inputV)

-- O (n² * k) 
bgstL :: V.Vector Int -> Int -> [Int]
bgstL inputV k = go [] inputV k where
    go bLst inputV_ k_
        | k_ /= 0 && bLstN /= bLst && sm > 0 = go bLstN sInputV (k_ - 1)
        | True = bLst
        where
        bLstN = bLst ++ [sm]
        (sm, si, lg) = findBgst inputV_
        sInputV = shorten inputV_ si lg

-- O (n * k)
bgstLKadane :: V.Vector Int -> Int -> [Int]
bgstLKadane inputV k = go [] inputV k where
    go bLst inputV_ k_
        | k_ /= 0 && bLstN /= bLst && sm > 0 = go bLstN sInputV (k_ - 1)
        | True = bLst
        where
        bLstN = bLst ++ [sm]
        (sm, si, lg) = kadane inputV_
        sInputV = shorten inputV_ si lg

main :: IO ()
main = do
    [n, k] <- readLst
    inputL <- readLst
    let inputV = V.fromList inputL
--    inputVM <- V.unsafeThaw inputV
--    print inputV
--    print $ findBgst inputV

    putStr $ unlines $ map show $ kadaneL inputV k
--    putStr $ unlines $ map show $ bgstLKadane inputV k
--    putStr $ unlines $ map show $ bgstL inputV k
{-
kadane $ V.fromList [9404,8036,-9334,-9146,8085,3024,988,5875,2264,-4643,-8916,-8072,1954,3424,5364,-2633,-8910,-7310,9443,-5096,4982,-7834,5164,-8360,185,265,277,-4154,-6615,6233,5988,-9008,5849,-948,6458,-9633,7955,432,1308,6533,-4667,9545,9446,1002,4452,-2285,-2413,-8734,4224,5492,-6250,-38,-3089,-6761,2326,-2209,-7962,-929,5710,-391,-6415,5399,4758,933,-3318,-8572,566,8181,-1512,-2937,-5897,5525,-7054,912,-8863,-4893,2963,-8827,-8376,5579,-8906,2265,5349,9388,4664,5708,2630,-4177,7665,6774,-5152,-5504,6138,2018,2464,3936,-5985,9804,-9520,1245]

100 20
9404 8036 -9334 -9146 8085 3024 988 5875 2264 -4643 -8916 -8072 1954 3424 5364 -2633 -8910 -7310 9443 -5096 4982 -7834 5164 -8360 185 265 277 -4154 -6615 6233 5988 -9008 5849 -948 6458 -9633 7955 432 1308 6533 -4667 9545 9446 1002 4452 -2285 -2413 -8734 4224 5492 -6250 -38 -3089 -6761 2326 -2209 -7962 -929 5710 -391 -6415 5399 4758 933 -3318 -8572 566 8181 -1512 -2937 -5897 5525 -7054 912 -8863 -4893 2963 -8827 -8376 5579 -8906 2265 5349 9388 4664 5708 2630 -4177 7665 6774 -5152 -5504 6138 2018 2464 3936 -5985 9804 -9520 1245
expected output:
47985
40945
20236
17440
11090
10742
9716
9443
8747
5710
5579
5525
5164
4982
2963
2326
1245
912
727

10 4
-10 -2 -4 -100 400 -20 -2 -1 -5 -10
exptected output:
400
-}
