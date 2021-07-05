module Main where
import Data.List (intercalate)
-- Complete the breakingRecords function below.
breakingRecords score = do
    [bMax, bMin] where
        bMax = snd $ foldl maxCnt ((head score), 0) score
        bMin = snd $ foldl minCnt ((head score), 0) score
        maxCnt :: (Num b, Ord a) => (a, b) -> a -> (a, b)
        maxCnt x y = if y > fst x 
                     then (y, snd x + 1)
                     else (fst x, snd x)
        minCnt :: (Num b, Ord a) => (a, b) -> a -> (a, b)
        minCnt x y = if y < fst x 
                     then (y, snd x + 1)
                     else (fst x, snd x)
readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    n <- readLn :: IO Int
    scoreTemp <- getLine
    let score = map (read :: String -> Int) . words $ scoreTemp
    let result = breakingRecords score
    putStrLn $ intercalate " " $ map (\x -> show x) $ result
    
-- sample input:
-- 4
-- 4 5 1 6

-- expected output:
-- 2 1
