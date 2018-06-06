module Main where

--
-- Complete the timeConversion function below.
--
timeConversion s = do
    let pm = if (s!!8) == 'P' then True else False
    let outStrLast = drop 2 $ take 8 s
    let hour :: Int
        hour = read $ take 2 s
    let hour24 = if pm && hour /= 12 
                     then hour + 12 
                 else if pm && hour == 12 || not pm && hour /= 12 
                     then hour 
                 else 0   -- not pm && hour == 12
    let hourStr = if (length $ show hour24) == 1 then "0" ++ show hour24 else show hour24
    hourStr ++ outStrLast

main :: IO()
main = do

    s <- getLine

    let result = timeConversion s
    putStr $ result

-- example input:
-- 12:00:34PM
