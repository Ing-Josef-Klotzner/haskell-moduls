--module ReverseFactorization where
import Data.List (sort, intercalate)

-- reduce input list to possible dividers
shortList :: Int -> [Int] -> [Int]
shortList n xL = reverse $ sort $ filter (\x -> n `div` x > 1) xL 

divi n xL = go xL [n] n where
    go [] rL rest
        | rest == 1 = rL
        | otherwise = []
    go xL rL rst
        | prev > 0 && inti == 0 = go xL ([prev] ++ rL) prev
        | prev < 1 || inti /= 0 = go (tail xL) rL rst
        where
            prev = rst `div` (head xL)
            inti = rst `mod` (head xL)

result_ n xL = divi n $ shortList n xL

result n xL
    | resul == [] = show (-1)
    | otherwise = intercalate " " $ map (\x -> show x) $ resul
    where
        resul = result_ n xL

main :: IO()
main = do
    dmTemp <- getLine
    let dm = words dmTemp
    let n = read (dm !! 0) :: Int
    let k = read (dm !! 1) :: Int
    sTemp <- getLine
    let aList = map (read :: String -> Int) . words $ sTemp
    putStrLn $ result n aList

--Sample Input 1

--15 5
--2 10 6 9 11
--Sample Output 1

---1
--Explanation 1

--Here no way exists so that we can reach  starting from .

--Sample Input 2

--72 9
--2 4 6 9 3 7 16 10 5
--Sample Output 2

--1 2 8 72
