import Control.Monad
perimeter :: [Double] -> Double
perimeter points = go (points ++ (take 2 points)) 0 where
    go :: [Double] -> Double -> Double
    go [] pm = pm
    go points pm = go (drop 2 points) (pm + (l (take 4 points)))
    l :: [Double] -> Double
    l [x1, y1, x2, y2] = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
    l _ = 0

main :: IO ()
main = do
    n <- fmap (read :: String -> Int) getLine
    ft <- forM [1..n] (\_ -> do fmap (map (read :: String -> Double).words) getLine)
    let f = concat ft
    print (perimeter f)

--sample input
--4
--0 0
--0 1  
--1 1  
--1 0

--out
--4

