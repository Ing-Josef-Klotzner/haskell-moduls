module Main where
import Data.List (sortBy, sort)
import Control.Monad (forM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
--import qualified Data.Array as A
import qualified Data.Set as Set
type IntV = Vector Int
type TstV = Vector Int

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

--findMinSet :: Int -> [Int] -> Int -> [Int] -> [Int]
findMinSet n intV t tstL = findMinSet_ n intV t tstV srtOrd
    where
    tstV = V.fromList srtdTstL
    -- sort tstL - remember sort order - on end sort back to orig. order
    (srtdTstL, srtOrd) = unzip $ sortBy srt (zip tstL [0..]) where
        srt (t,n) (t1,n1) = compare t t1

findMinSet_ :: Int -> IntV -> Int -> TstV -> [Int] -> [Int]
findMinSet_ n intV t tstV srtOrd = go 0 1 [] where
    go ti ptr resL
        | ti == t = bkSrtRslt
        | otherwise = go (ti + 1) ptrR (resL ++ [res]) 
        where
        (res, ptrR) = find ti ptr
        (bkSrtRslt, _) = unzip $ sortBy srt (zip resL srtOrd)
        srt (t,n) (t1,n1) = compare n n1
    find :: Int -> Int -> (Int, Int)
    find ti ptr = go ptr ptr lmt where
        lmt = V.length intV - 1
        go li i lu    -- li lower limit, lu upper limit
            | tstV V.! ti > intV V.! i && i == lmt = ((-1), i)
            | i == lu = (i, i)
            | tstV V.! ti <= intV V.! i = go li divD i
            | tstV V.! ti > intV V.! i = go i divU lu
            | otherwise = (i, i)
--            | tstV V.! ti <= intV V.! i = (i, i)
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
    tstL <- rdLn t
--    tstL_ <- getContents
--    let tstL = map read $ lines tstL_
--    putStrLn $ "size of int list: " ++ show n ++ " int list: " ++ show intL 
--        ++ "\ncount of test cases:" ++ show t ++ " test case list: " ++ show tstL
    let seqInOrd :: [Int]
        seqInOrd = sortBy sortGT intL -- sort in reverse (descending)
        v0 = V.singleton (0 :: Int)
        sortGT a b
          | a < b = GT
          | a > b = LT
          | a == b = EQ
        go vrm [] = return vrm
        go vrm seq = do
            let newV = V.snoc vrm newVal
                newVal = V.last vrm + head seq
            go newV (tail seq)  
    intV <- go v0 seqInOrd
    putStr $ unlines $ map show $ findMinSet n intV t tstL

{-
4
4 8 10 12
4
4
13
30
100
-}
