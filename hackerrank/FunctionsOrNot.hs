import Control.Monad
import Data.List (sort)

trd3 (_,_,c) = c

valid :: [(Int, Int)] -> Bool
valid f = trd3 $ foldl isValid (0,0,True) (sort f) where
    isValid (x1, x2, b) (y1, y2) = 
        if x1 == y1 && x2 /= y2 then 
            (y1,y2,False) 
        else (y1, y2, b)
main = do
    t <- fmap (read::String->Int) getLine
    forM [1..t] (\_->do
        n <- fmap (read::String->Int) getLine
        func <- forM [1..n] (\_->do fmap ((\[a, b]->(a,b)).map (read::String->Int).words) getLine :: IO (Int, Int))
        putStrLn $ if valid func then "YES" else "NO")

--Sample Input

--2
--3
--1 1
--2 2
--3 3
--4
--1 2
--2 4
--3 6
--4 8

--Sample Output

--YES  
--YES
