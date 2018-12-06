module Main where
import Data.List (subsequences, sortBy)
import Control.Monad (forM_)

-- get x lines
rdLn :: Int -> IO [Int]
rdLn 0 = return []
rdLn x = do
    cu <- readLn
    cs <- rdLn (x - 1)
    return ([cu] ++ cs)

findMinSet :: (Num b, Ord b) => t -> [b] -> Int -> [b] -> IO ()
findMinSet n intL t tstL = 
    forM_ [0..t-1] (\t_ -> do out (l t_))
        where
        out x = putStrLn $ (if x == [] then "-1" else getLen x)
        getLen x = show $ length $ fst $ head x
        l x = resL $ tstL !! x
        resL tx = sortBy srtByF $ filter filF $ map mapF $ fun
            where
            srtByF (x, sum) (x1, sum1) = compare (length x) (length x1)
            filF (x, sum) = sum >= tx
            mapF x = (x, sum x)
            fun = subsequences intL

main :: IO ()
main = do
    n <- readLn :: IO (Int)-- size of int list
    intL <- fmap (map (read :: String -> Int).words) getLine
    t <- readLn   -- number of testcases
    tstL <- rdLn t
--    putStrLn $ "size of int list: " ++ show n ++ " int list: " ++ show intL 
--        ++ "\ncount of test cases:" ++ show t ++ " test case list: " ++ show tstL
    findMinSet n intL t tstL
