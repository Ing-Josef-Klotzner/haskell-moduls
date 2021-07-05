import Control.Monad (forM, forM_)
import Data.List (intercalate, reverse, sort)
import Control.Monad.State (State, get, gets, put, modify, evalState)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (intToDigit)
import qualified Data.Set as Set

unique :: Ord a => [a] -> [a] 
unique xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

--      T  F  B  Bk L  R
dice = [1, 2, 6, 5, 3, 4]
diceToR dic = [dic !! 4, dic !! 1, dic !! 5, dic !! 3, dic !! 2, dic !! 0]
diceDwn dic = [dic !! 3, dic !! 0, dic !! 1, dic !! 2, dic !! 4, dic !! 5]

grid :: (Int, Int) -> [((Int, Int), Bool)]
grid (m, n) = [((x, y), b) | x <- [1..m], y <- [1..n], b <- [True]] 

gridMap :: (Int, Int) -> Map (Int, Int) Bool
gridMap (m, n) = M.fromList $ grid (m, n)

-- go diagonal
findPaths :: (Int, Int) -> ([[((Int, Int), Char)]], [Int])
findPaths (m, n) = go (1, 1) (gridMap (m, n)) [] [] [] 0 dice where
    go _ mpn _ _ _ _ _
        | mpn == M.empty = ([[]], [])
    go (0, 0) _ res _ dicSmL _ _ = (res, dicSmL) 
    go (x, y) mpn res path dicSmL sum dic
        | lookupTrue (x + 1, y) && x /= m = 
            go (x + 1, y) mpn res addXYP dicSmL sumN dicR 
        | lookupFalse (x + 1, y) && y /= n = 
            go (x, y + 1) setMapFalse res addXYP dicSmL sumN dicD
        | y == 1 && x == m && y /= n
            || lookupFalse ((x - 1), (y - 1)) && x == m && y /= n 
            || x == m && x == 1 && y /= n = 
            go (x, y + 1) setMapFalse res addXYP dicSmL sumN dicD
        | x == m && y /= n =
            go (x, y + 1) mpn res addXYP dicSmL sumN dicD
        -- end of finding one path   having found 4 different path length is sufficient
        | not enough && lookupTrue (1, (n - 1)) && x == m && y == n =
            go (1, 1) mpn ((rv addXYP) : res) [] (sumN : dicSmL) 0 dice
        -- end of finding all paths
        | enough && y == n && x == m =
            go (0, 0) mpn (rv ((rv addXYP) : res)) [] (rv (sumN : dicSmL)) 0 dice
        | lookupFalse (1, (n - 1)) && x == m && y == n =
            go (0, 0) mpn (rv ((rv addXYP) : res)) [] (rv (sumN : dicSmL)) 0 dice
        | y == 1 && y == n && x == m =
            go (0, 0) mpn (rv ((rv addXYP) : res)) [] (rv (sumN : dicSmL)) 0 dice
        where
            enough = (length $ unique (sumN : dicSmL)) == 4
            setMapFalse = M.adjust (== False) (x, y) mpn
            lookupTrue (v, w) = M.lookup (v, w) mpn == Just True
            lookupFalse (v, w) = M.lookup (v, w) mpn == Just False
            dicT = head dic
            dicR = diceToR dic
            dicD = diceDwn dic
            rv = reverse
            iTD = intToDigit
            sumN = sum + dicT
            addXYP = ((x, y), iTD dicT) : path

-- finding all variations of horizontal pattern
findPaths'' :: (Int, Int) -> ([[((Int, Int), Char)]], [Int])
findPaths'' (m, n) = go (1, 1) (gridMap (m, n)) [] [] [] 0 dice (m - 1) where
    go _ mpn _ _ _ _ _ _
        | mpn == M.empty = ([[]], [])
    go (0, 0) _ res _ dicSmL _ _ _ = (res, dicSmL) 
    go (x, y) mpn res path dicSmL sum dic z
        | lookupTrue (x + 1, y) && x /= m = 
            go (x + 1, y) mpn res addXYP dicSmL sumN dicR z
        | lookupFalse (x + 1, y) && y /= n = 
            go (x, y + 1) setMapFalse res addXYP dicSmL sumN dicD z
        | y == 1 && x == m && y /= n
            || lookupFalse ((x - z), (y - 1)) && x == m && y /= n 
            || x == m && x == 1 && y /= n = 
            go (x, y + 1) setMapFalse res addXYP dicSmL sumN dicD z
        | x == m && y /= n =
            go (x, y + 1) mpn res addXYP dicSmL sumN dicD z
        -- end of finding one path
        | lookupTrue (1, (n - 1)) && x == m && y == n =
            go (1, 1) mpn ((rv addXYP) : res) [] (sumN : dicSmL) 0 dice z
        -- end of finding all paths
        | lookupFalse (1, (n - 1)) && x == m && y == n && z /= 0 =
            go (1, 1) (gridMap (m, n)) ((rv addXYP) : res) [] (sumN : dicSmL) 0 dice (z - 1)
        | y == 1 && y == n && x == m && z /= 0 =
            go (1, 1) (gridMap (m, n)) ((rv addXYP) : res) [] (sumN : dicSmL) 0 dice (z - 1)
        | lookupFalse (1, (n - 1)) && x == m && y == n && z == 0 =
            go (0, 0) mpn (rv ((rv addXYP) : res)) [] (rv (sumN : dicSmL)) 0 dice z
        | y == 1 && y == n && x == m && z == 0 =
            go (0, 0) mpn (rv ((rv addXYP) : res)) [] (rv (sumN : dicSmL)) 0 dice z
        where
            setMapFalse = M.adjust (== False) (x, y) mpn
            lookupTrue (v, w) = M.lookup (v, w) mpn == Just True
            lookupFalse (v, w) = M.lookup (v, w) mpn == Just False
            dicT = head dic
            dicR = diceToR dic
            dicD = diceDwn dic
            rv = reverse
            iTD = intToDigit
            sumN = sum + dicT
            addXYP = ((x, y), iTD dicT) : path

getOrUpdate :: (Ord k) => k -> State (Map k v) v -> State (Map k v) v
getOrUpdate k ifEmptyState = do
    maybeVal <- gets (M.lookup k)
    case maybeVal of
        Just v -> return v
        Nothing -> do
            ifEmpty <- ifEmptyState
            modify (M.insert k ifEmpty)
            return ifEmpty

-- eauivalent to findWithDefault 
getW :: Ord k => a -> k -> Map k a -> a
getW ifEmpty k mp = do
    let maybeVal = M.lookup k mp
    case maybeVal of
        Just v -> v
        Nothing -> ifEmpty

-- function to show path
showPath :: (Int, Int) -> Int -> IO ()
showPath (m, n) p = showPath' (m, n) p findPaths
showPath'' :: (Int, Int) -> Int -> IO ()
showPath'' (m, n) p = showPath' (m, n) p findPaths''
showPath' :: (Ord t, Show t) => (Int, Int) -> Int -> ((Int, Int) -> ([[((Int, Int), Char)]], [t])) -> IO ()
showPath' (m, n) p findPaths = do
    let paths = fst $ findPaths (m, n)
        l = length paths
        px
            | p > l = 0
            | otherwise = p - 1 
    if p > l then
        putStrLn $ "pathnumber exceeds maximum ( 1 - " ++ show l ++ " ) -- set to 1!"
    else
        putStrLn $ "showing path number " ++ show p ++ " of ( 1 - " ++ show l ++ " )"
    let path = paths !! px
        pathMap = M.fromList path
        sumL = snd $ findPaths (m, n)
        prt scrMap = do 
            (zipWith id) (fmap (\x y -> getW ' ' (x,y) scrMap) (concat $ replicate n [1..(m + 1)])) 
                (sort $ concat $ replicate (m + 1) [1..n])
        screenMap = M.union pathMap blankScreenMap
    putStrLn $ prt screenMap
    putStrLn $ "this Sum: " ++ show (sumL !! px) ++ "   highest Sum: " ++ show (maximum  sumL)
    putStrLn "Maximum sum of all paths:"
    putStrLn $ show ( (zipWith id) ((\s p -> (s, p)) <$> sumL) [1..])
    where
        blankScreen :: [((Int, Int), Char)]
        blankScreen = [((x, y), b) | x <- [1..(m + 1)], y <- [1..n], b <- if x == (m + 1) then ['\n'] else ['-']] 

        blankScreenMap :: Map (Int, Int) Char
        blankScreenMap = M.fromList $ blankScreen

result' (m, n) = snd $ findPaths'' (m, n)
result (m, n)= maximum $ snd $ findPaths (m, n)
resultt (m, n)= maximum $ snd $ findPaths'' (m, n)

main :: IO()
main = do
    t <- fmap (read :: String -> Int) getLine
    nkL <- forM [1..t] (\_ -> do fmap (map (read :: String -> Int).words) getLine)
    
    putStrLn $ intercalate "\n" $ map (\[n, m] -> show (result (m, n))) $ nkL

--You are given an MxN grid and a 6 sided dice starting at the point (1, 1). You can only move dice toward right or down by rotating it in the respective direction. The value of the dice is the number of pips on the top face of it.

--Initial Grid

--If at ith step dice is rotated to right, then new configuration will be

--Top[i] = Left[i-1]
--Bottom[i] = Right[i-1]
--Left[i] = Bottom[i-1]
--Right[i] = Top[i-1]
--Front[i] = Front[i-1]
--Back[i] = Back[i-1]
--Similarly, if at ith step dice is rotated down, then new configuration will be

--Top[i] = Back[i-1]
--Bottom[i] = Front[i-1]
--Left[i] = Left[i-1]
--Right[i] = Right[i-1]
--Front[i] = Top[i-1]
--Back[i] = Bottom[i-1]
--Initially dice is at point (1, 1), and its top face has 1 pip, front face has 2 pips, and left face has 3 pips. 
--A path sum to a point is the sum of value of dice when it is rolled to that point from (1, 1). As already stated, value at the current location is the number of pips on the top face of the dice. Find the maximum path sum to (M, N).

--Note 
--The sum of pips at each pair of opposing sides is always 7.

--Input 
--The first line contains an integer, T, which denotes the number of test cases. T lines follow. 
--Each of these lines contains two space separated integers, M N, which represent the final point in the grid.

--Output 
--For each test case, print the sum of maximal path to (M, N).

--Constraints

--1 ≤ T ≤ 3600 
--1 ≤ M, N ≤ 60

--Sample Input #00

--4
--2 2
--1 2
--2 1
--3 3
--Sample Output #00

--9
--4
--6
--19
--Explanation 
--Case #00: There are two ways to reach (2, 2). Both's sum will be 9.

--Position :    (1, 1) -> (1, 2) -> (2, 2)
--Direction:          Right     Down
--Value    :      1    +    3    +    5     =    9
--Case #01: Dice has to roll toward right only one time.

--Position :    (1, 1) -> (1, 2)
--Direction:          Right
--Value    :      1    +    3      =    4
--Case #02: Dice has to roll down only one time.

--Position :    (1, 1) -> (2, 1)
--Direction:          Down
--Value    :      1    +    5      =    6
--Case #03: There are six ways in which dice can be rotated to (3, 3)

--Position :    (1, 1) -> (1, 2) -> (1, 3) -> (2, 3) -> (3, 3)
--Direction:          Right     Right     Down      Down
--Value    :      1    +    3    +     6   +    5    +    1    = 16

--Position :    (1, 1) -> (1, 2) -> (2, 2) -> (2, 3) -> (3, 3)
--Direction:          Right     Down      Right     Down
--Value    :      1    +    3    +     5   +    6    +    4    = 19

--Position :    (1, 1) -> (1, 2) -> (2, 2) -> (3, 2) -> (3, 4)
--Direction:          Right     Down      Down      Right
--Value    :      1    +    3    +     5   +    4    +    6    = 19

--Position :    (1, 1) -> (2, 1) -> (2, 2) -> (2, 3) -> (3, 3)
--Direction:          Down      Right     Right     Down
--Value    :      1    +    5    +     3   +    2    +    6    = 17

--Position :    (1, 1) -> (2, 1) -> (2, 2) -> (3, 2) -> (3, 3)
--Direction:          Down      Right     Down      Right
--Value    :      1    +    5    +     3   +    6    +    2    = 17

--Position :    (1, 1) -> (2, 1) -> (3, 1) -> (3, 2) -> (3, 3)
--Direction:          Down      Down      Right     Right
--Value    :      1    +    5    +     6   +    3    +    1    = 16
--So (Right, Down, Right, Down) or (Right, Down, Down, Right) will be best rotations for this case.

