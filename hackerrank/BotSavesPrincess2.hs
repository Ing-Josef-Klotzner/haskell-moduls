module Main where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

princessPosY :: Foldable t => [t Char] -> Int
princessPosY grid = fromMaybe 0 $ findIndex (== True) (map (elem 'p') grid)  -- which element is True? -> y

princessPosX :: Int -> [[Char]] -> Int
princessPosX yp grid = fromMaybe 0 $ findIndex (== 'p') (grid !! yp)

nextMove :: Int -> Int -> Int -> Int -> String
nextMove x y xp yp
    | x > xp = "LEFT"
    | x < xp = "RIGHT"
    | x == xp && y > yp = "UP"
    | x == xp && y < yp = "DOWN"
    | otherwise = "ERROR"

main = do

    n <- getLine
    xy <- getLine
    let yxw = words xy
    let y = read (yxw !! 0)
    let x = read (yxw !! 1)
--    putStrLn $ y ++ " " ++ x
    
    let i = read n
    grid <- getList i
    let yp = princessPosY grid
    let xp = princessPosX yp grid
--    putStrLn $ show xp
--    putStrLn $ show yp
    putStrLn $ nextMove x y xp yp
    
--    sample input:
--    5
--    2 3
--    -----
--    -----
--    p--m-
--    -----
--    -----
