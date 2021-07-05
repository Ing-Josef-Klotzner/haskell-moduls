f :: Int -> [Int] -> [Int]
f n arr = foldr (go n) [] arr where -- Complete this function
    go 0 x m = m
    go y x m = x : (go (y - 1) x m)
-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words

-- sample input:
--3
--1
--2
--3
--4

--Sample Output

--1
--1
--1
--2
--2
--2
--3
--3
--3
--4
--4
--4
