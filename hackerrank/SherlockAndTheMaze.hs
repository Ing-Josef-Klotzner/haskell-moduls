module Main where
import Data.List (intercalate)
import Control.Monad (forM)
import Data.Array
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sort)
import Control.Concurrent.MVar (newMVar, readMVar, putMVar, takeMVar)
import Data.Ix (Ix, range)

grid :: (Int, Int) -> [((Int, Int), Bool)]
grid (m, n) = [((x, y), b) | x <- [1..m], y <- [1..n], b <- [True]] 

gridMap :: (Int, Int) -> Map (Int, Int) Bool
gridMap (m, n) = M.fromList $ grid (m, n)

findpaths :: (Int, Int, Int) -> [[((Int, Int), Int)]]
findpaths (m, n, k) = go (1, 1) [] [] 0 True where
    go (0, 0) res _ _ _ = res
    go (x, y) res path k_ right
        | x /= m = go (x + 1, y) res addXYP k_nr True
        | y /= n = go (x, y + 1) res addXYP k_nd False
        | x == m && y == n = go (0, 0) add_ini [] 0 right
        where
            addXYP = path ++ [((x, y), k_)]
            add_ini = if k_ > k  then res else res ++ [addXYP]
            k_nr = if x /= 1 || y /= 1 then (if right then k_ else k_ + 1) else k_
            k_nd = if x /= 1 || y /= 1 then (if right then k_ + 1 else k_) else k_

table_array2D :: (Enum t, Enum t1, Num t, Num t1, Ix t, Ix t1) =>
                (t, t1) -> (((t, t1) -> e) -> (t, t1) -> e) -> (t, t1) -> e
table_array2D (m, n) f = g
    where
        arr = array ((1, 1), (m, n)) [((x,y), f g (x, y)) | x <- [1..m], y <- [1..n]]
        g (x, y) = arr ! (x, y)

--table_array :: Ix i => (i, i) -> ((i -> e) -> i -> e) -> i -> e
table_array :: Ix a => (a, a) -> ((a -> b) -> a -> b) -> (a -> b)
table_array dom f = g
    where
        arr = array dom [(x, f g x) | x <- range dom]
        g x = arr ! x

routes :: (Int, Int) -> Int
routes (m, n) = arr ! (m, n)
    where
        arr = array ((1, 1), (m, n)) 
                    [((x, y), inner (x, y)) | x <- [1..m], y <- [1..n]]
        inner (x, y)
            | x == 1 && y == 1 = 0
            | x == 1 || y == 1 = 1
            | otherwise = arr ! (x - 1, y) + arr ! (x, y - 1)

--routes :: (Int, Int) -> Int
--routes (m, n) = arr ! (m, n)
--    where
--        arr = array ((1, 1), (m, n)) [((x, y), inner (x, y)) | x <- [1..m], y <- [1..n]]
--        inner (x, y)
--            | x == 1 && y == 1 = 0
--            | x == 1 || y == 1 = 1
--            | otherwise = arr ! (x - 1, y) + arr ! (x, y - 1)

knapsack :: (Num a, Num b, Ord b, Ix a) => [(b, a)] -> a -> b
knapsack items wmax = table_array (0, wmax) m wmax
    where
        m _ 0 = 0
        m self w = maximum $ 0:[vi + self (w - wi) | (vi, wi) <- items, wi <= w]

-- knapsack [(4, 12), (2, 2), (2, 1), (1, 1), (10, 4)] 15

memoIO :: Ord k => (k -> a) -> IO (k -> IO a)
memoIO f = do
  v <- newMVar M.empty
  let f' x = do
        m <- readMVar v
        case M.lookup x m of
          Nothing -> do let { r = f x }; m <- takeMVar v; putMVar v (M.insert x r m); return r
          Just r  -> return r
  return f'

-- finding all variations of horizontal pattern
findPaths :: (Int, Int, Int) -> [[((Int, Int), Int)]]
findPaths (m, n, k) = go (1, 1) (gridMap (m, n)) [] [] 0 True (m - 1) True where
    go _ mpn _ _ _ _ _ _
        | mpn == M.empty = [[]]
    go (0, 0) _ res _ _ _ _ _ = res 
    go (x, y) mpn res path k_ right z h
        | lookupTrue (x + 1, y) && x /= m && h = 
            go (x + 1, y) mpn res addXYP k_nr True z h
        | lookupTrue (x, y + 1) && y /= n && not h = 
            go (x, y + 1) mpn res addXYP k_nd False z h
        | lookupFalse (2, y) && y /= n && h = 
            go (x, y + 1) setMapFalse res addXYP k_ False z h
        | lookupFalse (x, 2) && x /= m && not h = 
            go (x + 1, y) setMapFalse res addXYP k_ True z h
        | lookupFalse (x + 1, y) && y /= n && h = 
            go (x, y + 1) setMapFalse res addXYP k_nd False z h
        | lookupFalse (x, y + 1) && x /= m && not h = 
            go (x + 1, y) setMapFalse res addXYP k_nr True z h

        | y == 1 && x == m && y /= n && h
            || lookupFalse ((x - z), (y - 1)) && x == m && y /= n && h =
            go (x, y + 1) setMapFalse res addXYP k_nd False z h
        | x == 1 && x /= m && y == n && not h
            || lookupFalse ((x - 1), (y - z)) && x /= m && y == n && not h =
            go (x + 1, y) setMapFalse res addXYP k_nr True z h
        | x == m && y /= n && h =
            go (x, y + 1) mpn res addXYP k_nd False z h
        | x /= m && y == n && not h =
            go (x + 1, y) mpn res addXYP k_nr True z h
        -- end of finding one path
        | lookupTrue (1, (n - 1)) && x == m && y == n && h =
            go (1, 1) mpn add_ini [] 0 True z h
        | lookupTrue ((m - 1), 1) && x == m && y == n && not h =
            go (1, 1) mpn add_ini [] 0 False z h
        -- end of finding all paths
        | lookupFalse (1, (n - 1)) && x == m && y == n && z /= 0 && h =
            go (1, 1) (gridMap (m, n)) add_ini [] 0 True (z - 1) h
        | y == 1 && y == n && x == m =
            go (0, 0) mpn (rv add_ini) [] 0 right z h
        | lookupFalse ((m - 1), 1) && x == m && y == n && z /= 0 && not h =
            go (1, 1) (gridMap (m, n)) add_ini [] 0 False (z - 1) h
        | x == 1 && y == n && x == m =
            go (0, 0) mpn (rv add_ini) [] 0 right z h
        | lookupFalse (1, (n - 1)) && x == m && y == n && z == 0 && h =
            go (1, 1) (gridMap (m, n)) add_ini [] 0 False (n - 1) False
        | lookupFalse ((m - 1), 1) && x == m && y == n && z == 0 && not h =
            go (0, 0) mpn (rv add_ini) [] 0 False z h
        where
            setMapFalse = M.adjust (== False) (x, y) mpn
            lookupTrue (v, w) = M.lookup (v, w) mpn == Just True
            lookupFalse (v, w) = M.lookup (v, w) mpn == Just False
            rv = reverse
            add_ini = if rv addXYP `elem` res || k_ > k  then res else (rv addXYP) : res
            addXYP = ((x, y), k_) : path
            k_nr = if right then k_ else k_ + 1
            k_nd = if right then k_ + 1 else k_

-- eauivalent to findWithDefault 
getW :: Ord k => a -> k -> Map k a -> a
getW ifEmpty k mp = do
    let maybeVal = M.lookup k mp
    case maybeVal of
        Just v -> v
        Nothing -> ifEmpty

-- function to show path
showPath :: (Int, Int, Int) -> Int -> IO ()
showPath (m, n, k) p = showPath' (m, n, k) p findPaths
showPath' :: (Int, Int, Int) -> Int -> ((Int, Int, Int) -> [[((Int, Int), Int)]]) -> IO ()
showPath' (m, n, k) p findPaths = do
    let paths = findPaths (m, n, k)
        l = length paths
        px
            | p == 0 = 0 
            | p > l = 0
            | otherwise = p - 1 
    if p > l || p == 0 then
        putStrLn $ "pathnumber exceeds range ( 1 - " ++ show l ++ " ) -- set to 1!"
    else
        putStrLn $ "showing path number " ++ show p ++ " of ( 1 - " ++ show l ++ " )"
    let path_ = paths !! px
        path = map (\((x, y), k) -> if k >= 0 then ((x,y), 'x') else ((x,y), ' ')) path_
        pathMap = M.fromList path
--        sumL = snd $ findPaths (m, n)
        prt scrMap = do 
            (zipWith id) (fmap (\x y -> getW ' ' (x,y) scrMap) (concat $ replicate n [1..(m + 1)]))
                (sort $ concat $ replicate (m + 1) [1..n])
        screenMap = M.union pathMap blankScreenMap
    putStrLn $ prt screenMap
--    putStrLn $ "this Sum: " ++ show (sumL !! px) ++ "   highest Sum: " ++ show (maximum  sumL)
--    putStrLn "Maximum sum of all paths:"
--    putStrLn $ show ( (zipWith id) ((\s p -> (s, p)) <$> sumL) [1..])
    where
        blankScreen :: [((Int, Int), Char)]
        blankScreen = [((x, y), b) | x <- [1..(m + 1)], y <- [1..n], b <- if x == (m + 1) then ['\n'] else ['-']] 

        blankScreenMap :: Map (Int, Int) Char
        blankScreenMap = M.fromList $ blankScreen

main :: IO()
main = do
    t <- fmap (read :: String -> Int) getLine
    nkL <- forM [1..t] (\_ -> do fmap (map (read :: String -> Int).words) getLine)
    
    putStrLn $ intercalate "\n" $ map (\[n, m, k] -> show (length $ findPaths (n, m, k))) $ nkL

--10
--1 3 3
--1 2 4
--4 2 3
--2 3 4
--2 5 3
--2 1 5
--4 4 3
--2 2 2
--2 2 1
--2 1 2

--1
--1
--4
--3
--5
--1
--14
--2
--2
--1

--Watson gives a 2-D grid to Sherlock. Rows are numbered 1 to N from top to bottom and columns are numbered 1 to M from left to right. Sherlock is at position (1,1) right now and he is free to face any direction before he starts to move. He needs to reach (N,M). In one step, he can either move downwards or rightwards. Also, he cannot make more than K turns during his whole journey.

--There are two possible scenarios when a turn can occur at point (i, j):

--Turns Right: (i-1, j)  ->  (i, j)  ->  (i, j+1)
--                      Down        Right

--Turns Down:  (i, j-1)  ->  (i, j)  ->  (i+1, j)
--                     Right        Dowm
--Given N, M and K, help him by printing the number of ways to reach (N,M) with at most K turns. As this value can be very large, print the answer modulo (109 + 7).

--Input 
--First line contains T, the number of testcases. Then T lines follow, where each line represents a test case. Each testcase consists of three space separated integers, N M K, where (N, M) is the final location and K is the maximum number of allowed turns.

--Output 
--For each testcase, print the required answer in one line.

--Constraints 
--1 ≤ T ≤ 10 
--1 ≤ N, M ≤ 100 
--0 ≤ K ≤ 100

--Note

--He can take at most K turns.
--He is free to face any direction before starting from (1, 1).
--Sample Input

--3
--2 2 3
--2 3 1
--4 4 4
--Sample Output

--2
--2
--18
--Sample explanation 
--Test Case #00: There is no way to reach (2, 2) with 0, 2 or 3 turns. He will always reach (2, 2) with 1 turn only. There are two ways shown below:

--He starts from (1, 1) facing right and moves to (1, 2). Then he faces down and moves to (2, 2).
--He starts from (1, 1) facing down and moves to (2, 1). Then he turns right and moves to (2, 2).
--Test Case #01: He can't reach (2, 3) with 0 turns. There are only two ways to reach (2, 3) with exactly 1 turn.

--He starts from (1, 1) facing down and moves to (2, 1). Then he turns right and takes two steps forward to reach (2, 3).
--He starts from (1, 1) facing right and moves two steps forward to reach (1, 3). Then he turns down and proceeds one step to (2, 3).
--Test Case #02: There are 0 ways with 0 turn, 2 ways with 1 turn, 4 ways with 2 turns, 8 ways with 3 turns and 4 ways with 4 turns to reach (4, 4).
