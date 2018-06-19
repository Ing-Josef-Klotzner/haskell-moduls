module Main where
import Data.List (sort)
-- Complete the getTotalX function below.
getTotalX a b = do
    let c = [(last a) .. (head b)]
    let allAareFactor x =  foldr (\y z -> (x `rem` y == 0) && z) True a
    let isFactorB x = foldr (\y z -> (y `rem` x == 0) && z) True b
    length $ filter isFactorB $filter allAareFactor c

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    nmTemp <- getLine
    let nm = words nmTemp
    let n = read (nm !! 0) :: Int
    let m = read (nm !! 1) :: Int
    aTemp <- getLine
    let a = sort $ map (read :: String -> Int) . words $ aTemp
    bTemp <- getLine
    let b = sort $ map (read :: String -> Int) . words $ bTemp
    let total = getTotalX a b
    putStrLn $ show total
