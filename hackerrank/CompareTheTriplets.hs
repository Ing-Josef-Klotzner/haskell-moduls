module Main where
-- Complete the solve function below.
solve a b = do
    let resA = sum $ zipWith (\a b -> if a > b then 1 else 0) a b
    let resB = sum $ zipWith (\a b -> if a < b then 1 else 0) a b
    show (resA) ++ " " ++ show (resB)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    aTemp <- getLine
    let a = map (read :: String -> Int) . words $ aTemp
    bTemp <- getLine
    let b = map (read :: String -> Int) . words $ bTemp
    let result = solve a b
    putStrLn result
