f :: Int -> [Int] -> [Int]
f n arr = fltr (< n) arr where --Fill up this function
--    fltr fl lis = foldr (\x y -> if fl x then x : y else y) [] lis
    fltr fl lis = [x | x <- lis, fl x]
-- The Input/Output section. You do not need to change or modify this part
main = do 
    n <- readLn :: IO Int 
    inputdata <- getContents 
    let 
        numbers = map read (lines inputdata) :: [Int] 
    putStrLn . unlines $ (map show . f n) numbers


--Sample Input

--3
--10
--9
--8
--2
--7
--5
--1
--3
--0
--Sample Output

--2
--1
--0
