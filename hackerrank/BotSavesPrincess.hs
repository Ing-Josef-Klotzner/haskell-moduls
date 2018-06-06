module Main where
getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

timesDir x dir = strg x where
    strg 0 = ""
    strg x = dir ++ strg (x - 1)
    
displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess i grid
    | ((grid!!0)!!0)== 'p' = (timesDir (i `quot` 2) "UP\n") ++ (timesDir (i `quot` 2) "LEFT\n")
    | ((grid!!0)!!(i - 1)) == 'p' = (timesDir (i `quot` 2) "UP\n") ++ (timesDir (i `quot` 2) "RIGHT\n")
    | ((grid!!(i-1))!!0) == 'p' = (timesDir (i `quot` 2) "DOWN\n") ++ (timesDir (i `quot` 2) "LEFT\n")
    | ((grid!!(i-1))!!(i-1)) == 'p' = (timesDir (i `quot` 2) "DOWN\n") ++ (timesDir (i `quot` 2) "RIGHT\n")

main = do
    n <- getLine
    let i = read n
    grid <- getList i
    putStrLn.displayPathtoPrincess i $ grid

-- input:
-- 3
-- ---
-- -m-
-- p--
