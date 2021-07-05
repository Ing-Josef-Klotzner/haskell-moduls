import Control.Monad (forM, forM_)
import Data.List (intercalate)

factorial :: Int -> Integer
factorial 0 = 1
factorial n = (scanl (*) 1 [2..]) !! (n - 1)

factorial' :: Int -> Integer
factorial' = product . flip take [1..]

f = factorial'

-- C(n,k)=n!/(k!(n−k)!) 
cnt (n, k) = f n `div` (f k * f (n - k)) `mod` (10^8+7)

main :: IO()
main = do
    t <- fmap (read :: String -> Int) getLine
    nkL <- forM [1..t] (\_ -> do fmap (map (read :: String -> Int).words) getLine)
    
    putStrLn $ intercalate "\n" $ map (\[n,k] -> show (cnt (n, k))) $ nkL



--Input 
--First line of input will contain a integer, T, representing the number of test cases. Then T lines follow, each representing a test case. In each test case, there are two space separated integers, N K, total number of lemurs available and number of lemurs to be selected.

--Output 
--For each test case, print total number of different teams that can be formed. As this number be large, print answer modulo (10^8+7).

--Constraints 
--1 <= T <= 1000 
--1 <= N <= 1000 
--0 <= K <= N

--Sample Input

--5
--2 1
--5 1
--5 2
--5 3
--10 5 

--Sample Output

--2
--5
--10
--10
--252

--Explanation 
--Test case #1: You have to select one of two lemur {a, b}. Any of them can be selected, {{a}, {b}}. 
--Test case #2: Similarly you have to select any one out of five lemurs {a, b, c, d, e}. There are 5 ways of doing that, {{a}, {b}, {c}, {d}, {e}}. 
--Test case #3: You have to select two lemurs out of five {a, b, c, d, e}. These are the possible teams: {{a, b}, {a, c}, {a, d}, {a, e}, {b, c}, {b, d}, {b, e}, {c, d}, {c, e}, {d, e}}. 
--Test case #4: Selecting three out of five lemurs is equivalent rejecting two of them. So it will be equal to number of ways of selecting two of five lemurs. 
--Test case #5: There can be 252 different teams formed by selecting 5 out of 10 lemurs.

