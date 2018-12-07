module Main where
import Data.List (sortBy, sort)
import Control.Monad (forM_)
-- import Data.Vector (Vector)
import qualified Data.Array as A
--import qualified Data.Vector as V

-- get x lines
rdLn :: Int -> IO [Int]
rdLn 0 = return []
rdLn x = do
    cu <- readLn
    cs <- rdLn (x - 1)
    return ([cu] ++ cs)

findMinSet :: (Num b, Ord b) => t -> [b] -> Int -> [b] -> IO ()
findMinSet n intL t tstL = forM_ [0..t-1] (\t_ -> do out (l t_))
    where
    out x = putStrLn $ (if x == [] then "-1" else getLen x)
    getLen x = show $ length $ fst $ head x
    l x = resL $ tstL !! x
    resL tx = sortBy srtByF $ filter filF $ map mapF $ fun_
        where
        srtByF (x, sum) (x1, sum1) = compare (length x) (length x1)
        filF (x, sum) = sum >= tx
        mapF x = (x, sum x)
        fun_ = go 1 [] where
            go i funL
                | length intL == i = funL
                | otherwise = go (i + 1) (take i seqInOrd : funL)
        seqInOrd = reverse $ sort intL

fun l = go 1 [] where
    go i funL
        | length l == i = funL
        | otherwise = go (i + 1) (take i seqInOrd : funL)
    seqInOrd = reverse $ sort l

fun1 l = go 1 [] where
    go i funL
        | length l == i = funL
        | otherwise = go (i + 1) (take i seqInOrd : funL)
    seqInOrd = reverse $ sort l

-- intLV = V.fromList [4,5,7,8,33,11,12,21,25]
intLi = [4,5,7,8,33,11,12,21,25]

main :: IO ()
main = do
    n <- readLn :: IO (Int)-- size of int list
    intL <- fmap (map (read :: String -> Int).words) getLine
    t <- readLn   -- number of testcases
    tstL <- rdLn t
--    putStrLn $ "size of int list: " ++ show n ++ " int list: " ++ show intL 
--        ++ "\ncount of test cases:" ++ show t ++ " test case list: " ++ show tstL
    findMinSet n intL t tstL

{-
4
4 8 10 12
4
4
13
30
100
-}
