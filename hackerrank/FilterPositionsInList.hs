f :: [Int] -> [Int]
f lst = go (length lst - 1) where -- Fill up this Function
    go 0 = []
    go (1) = [(lst !! 1)]
    go x = if odd x then (go (x - 1) ++ [(lst !! x)]) else go (x - 1)
-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
    inputdata <- getContents
    mapM_ (putStrLn. show). f. map read. lines $ inputdata

--Sample Input

--2
--5
--3
--4
--6
--7
--9
--8
--Sample Output

--5
--4
--7
--8
