module Main where

printStaircase n = go n where
    go 0 = ""
    go x = replicate (x - 1) ' ' ++ replicate (n - x + 1) '#' ++ "\n" ++ go (x - 1) 

-- Complete the staircase function below.
staircase n = do
    putStrLn $ printStaircase n

main :: IO()
main = do
    n <- readLn :: IO Int
    staircase n

