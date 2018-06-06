module Main where

-- Complete the solve function below.
solve n s d m = 
    if n > 1  && m <= n
        then go (length s - m) 
    else if d == head s 
        then 1 
    else 0
    where
        go (-1) = 0
        go x = if sum (take m $ drop x s) == d 
               then go (x - 1) + 1 
               else go (x - 1) 

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    n <- readLn :: IO Int
    sTemp <- getLine
    let s = map (read :: String -> Int) . words $ sTemp
    dmTemp <- getLine
    let dm = words dmTemp
    let d = read (dm !! 0) :: Int
    let m = read (dm !! 1) :: Int
    let result = solve n s d m
    putStrLn $ show result


-- sample input:
--5
--1 2 1 3 2
--3 2

-- sample output:
-- 2
