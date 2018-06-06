module Main where

-- Complete the countApplesAndOranges function below.
countApplesAndOranges s t a b apples oranges = do
    putStrLn $ show $ sum $ map (\x -> if ((a + x) >= s && (a + x) <= t ) then 1 else 0) apples
    putStrLn $ show $ sum $ map (\x -> if ((b + x) >= s && (b + x) <= t ) then 1 else 0) oranges

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stTemp <- getLine
    let st = words stTemp
    let s = read (st !! 0) :: Int
    let t = read (st !! 1) :: Int
    abTemp <- getLine
    let ab = words abTemp
    let a = read (ab !! 0) :: Int
    let b = read (ab !! 1) :: Int
    mnTemp <- getLine
    let mn = words mnTemp
    let m = read (mn !! 0) :: Int
    let n = read (mn !! 1) :: Int
    appleTemp <- getLine
    let apple = map (read :: String -> Int) . words $ appleTemp
    orangeTemp <- getLine
    let orange = map (read :: String -> Int) . words $ orangeTemp
    countApplesAndOranges s t a b apple orange
