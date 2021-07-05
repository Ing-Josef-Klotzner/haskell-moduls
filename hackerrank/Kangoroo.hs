module Main where

-- Complete the kangaroo function below.
--   4 = 0  3  4  2    =>   YES
--  -5 = 0  2  5  3    =>   NO
--26.6 = 21 6  47 3    =>   NO   (not integer)
--   t = (x2  - x1) / (v1 - v2)
kangaroo x1 v1 x2 v2
    | x2 - x1 <= 0 || v1 - v2 <= 0 = "NO"
    | v1 - v2 /= 0 = if (x2 - x1) `rem` (v1 - v2) > 0 then "NO" else "YES"
    | otherwise = "YES"

main :: IO()
main = do
    x1V1X2V2Temp <- getLine
    let x1V1X2V2 = words x1V1X2V2Temp
    let x1 = read (x1V1X2V2 !! 0) :: Int
    let v1 = read (x1V1X2V2 !! 1) :: Int
    let x2 = read (x1V1X2V2 !! 2) :: Int
    let v2 = read (x1V1X2V2 !! 3) :: Int
    let result = kangaroo x1 v1 x2 v2
    putStrLn result
