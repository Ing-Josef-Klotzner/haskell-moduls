import Text.Printf (printf)

-- This function should return a list [area, volum10e].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, volume] where --Complete this function--
    area = go n where
        go 0 = 0
        go y = f (cx y) * dx + go (y - 1)
    f x = go (length a - 1) where
        go (-1) = 0
        go y = fromIntegral (a!!y) * x ** (fromIntegral (b!!y)) + go (y - 1)
    volume = go n where
        go 0 = 0
        go y = pi * ((f (cx y)) ** 2) * dx + go (y - 1)
--    f :: Floating a => a -> a 
--    f x = sum [fromIntegral a' * (x ** fromIntegral b') | (a', b') <- zip a b]    
    
    dx = 0.001 :: Double
    n = fromIntegral (r - l) / dx
--    i = 1 :: Double   -- only for test
    cx i = fromIntegral l + dx * i -- - dx / 2   -- i = 1 .. n    dx/2 .. to get middle of dx
--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
--    let a = [-1, 2, 0, 2, -1, -3, -4, -1, -3, -4, -999, 1, 2, 3, 4, 5]
--        b = [-15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0]
--        l = 1
--        r = 10
--    putStrLn $ show (solve l r a b)
--Sample Input

--1 2 3 4 5
--6 7 8 9 10
--1 4  
--Explanation

--The algebraic expression represented by:


--We need to find the area of the curve enclosed under this curve, between the limits  and . We also need to find the volume of the solid formed by revolving this curve around the x-axis between the limits x=1 and 4.

--Sample Output

--2435300.3
--26172951168940.8

--in
---1 2 0 2 -1 -3 -4 -1 -3 -4 -999 1 2 3 4 5
---15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0
--1 10
--out
---193.1
--336642.8
