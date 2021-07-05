{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad

-- merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x > y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

f [] = []
f ns
    | v < 0 = (-v) : merge (f (take l ns)) (f (drop (r + 1) ns))
    | otherwise = []
  where
    (v, l, r) = go (0, 0, 0) (0, 0, 0) $ zip [0..] ns
    go !(endHere, l, r) !whole ns = case ns of
        [] -> whole
        ((i, x):xs) ->
            let cur = min (endHere-x, l, i) (0, i+1, i)
            in go cur (min cur whole) xs

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine
    ns <- map read . words <$> getLine
    
    mapM_ print $ take k $ f ns
