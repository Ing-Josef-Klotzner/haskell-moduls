import Control.Monad
area :: [Double] -> Double
area points = go (drop 2 points) (take 2 points) 0 where
    go :: [Double] -> [Double] -> Double -> Double
    go [] [xr, yr] ar = ar
    go points [xr, yr] ar = go (drop 2 points) [xr, yr] (ar + (a [xr, yr] (take 4 points)))
    a :: [Double] -> [Double] -> Double
    a [xr, yr] [x1, y1, x2, y2] = (xr * (y1 - y2) + x1 * (y2 - yr) + x2 * (yr - y1)) / 2
    a _ _ = 0
-- calculating area by creating triangles from first point as reference against pairs of all succeding and
-- add all areas of those triangles
-- area of triangle by using trapez method

main :: IO ()
main = do
    n <- fmap (read :: String -> Int) getLine
    ft <- forM [1..n] (\_ -> do fmap (map (read :: String -> Double).words) getLine)
    let f = concat ft
    print (area f)

--Sample Input

--4
--0 0
--0 1  
--1 1  
--1 0

--Sample Output

--1
