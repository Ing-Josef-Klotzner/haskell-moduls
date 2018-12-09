module Main where
import Data.List (sortBy, sort)
import Control.Monad (forM)
--import Data.Vector (Vector)
--import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.Set as Set
type IntArr = A.Array Int Int
type TstArr = A.Array Int Int

unique :: (Ord a) => [a] -> [a]
unique xs = go Set.empty xs where
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise        = x : go (Set.insert x s) xs
    go _ _                 = []

-- get x lines
rdLn :: Int -> IO [Int]
rdLn 0 = return []
rdLn x = do
    cu <- readLn
    cs <- rdLn (x - 1)
    return ([cu] ++ cs)

findMinSet :: Int -> [Int] -> Int -> [Int] -> [Int]
findMinSet n intL t tstL = findMinSet_ n funArr t tstA srtOrd
    where
    tstA = A.array (0,t - 1) (zip [0..t - 1] (srtdTstL))
    -- sort tstL - remember sort order - on end sort back to orig. order
    (srtdTstL, srtOrd) = unzip $ sortBy srt (zip tstL [0..]) where
        srt (t,n) (t1,n1) = compare t t1
    funArr :: IntArr
    funArr = go 1 arr seqInOrd where
        go i arrm [] = arrm
        go i arrm seq = go (i + 1) newArr (tail seq)
            where
            newArr = arrm A.// [(i, newVal)]
            newVal = arrm A.! (i - 1) + (head seq)
        seqInOrd = sortBy sortGT intL  -- sort in reverse (descending)
        arr = A.array (0,n) [(i,0) | i <- [0..n]]
        sortGT a b
          | a < b = GT
          | a > b = LT
          | a == b = EQ

findMinSet_ :: Int -> IntArr -> Int -> TstArr -> [Int] -> [Int]
findMinSet_ n intA t tstA srtOrd = go 0 1 [] where
    go ti ptr resL
        | ti == t = bkSrtRslt
        | otherwise = go (ti + 1) ptrR (resL ++ [res]) 
        where
        (res, ptrR) = find ti ptr
        (bkSrtRslt, _) = unzip $ sortBy srt (zip resL srtOrd)
        srt (t,n) (t1,n1) = compare n n1
    find :: Int -> Int -> (Int, Int)
    find ti ptr = go ptr ptr lmt where
        lmt = snd $ A.bounds intA
        go li i lu    -- li lower limit, lu upper limit
            | tstA A.! ti > intA A.! i && i == lmt = ((-1), i)
            | i == lu = (i, i)
            | tstA A.! ti <= intA A.! i = go li divD i
            | tstA A.! ti > intA A.! i = go i divU lu
            | otherwise = (i, i)
--            | tstA A.! ti <= intA A.! i = (i, i)
--            | otherwise = go (i + 1)
            where
            divD = div (li + i + mod') 2
            mod' = mod (li + i) 2
            divU = div (i + lu + mod_) 2
            mod_ = mod (i + lu) 2

main :: IO ()
main = do
    n <- readLn :: IO (Int)-- size of int list
    intL <- fmap (map (read :: String -> Int).words) getLine
    t <- readLn   -- number of testcases
--    tstL <- rdLn t
    tstL_ <- getContents
    let tstL = map read $ lines tstL_
--    putStrLn $ "size of int list: " ++ show n ++ " int list: " ++ show intL 
--        ++ "\ncount of test cases:" ++ show t ++ " test case list: " ++ show tstL
    putStr $ unlines $ map show $ findMinSet n intL t tstL

{-
4
4 8 10 12
4
4
13
30
100
-}
