import Control.Monad (forM, forM_)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

--      T  F  B  Bk L  R
dice = [1, 2, 6, 5, 3, 4]
diceToR dic = [dic !! 4, dic !! 1, dic !! 5, dic !! 3, dic !! 2, dic !! 0]
diceDwn dic = [dic !! 3, dic !! 0, dic !! 1, dic !! 2, dic !! 4, dic !! 5]

--grid :: (Enum t1, Num t1) => (t, t1) -> [(t1, t1, Bool)]
--grid (m, n) = [(x, y, b) | y <- [1..(n - 1)], x <- [2..n], b <- [True]]
grid :: (Int, Int) -> [((Int, Int), Bool)]
grid (m, n) = [((x, y), b) | x <- [1..m], y <- [1..n], b <- [True]] 

gridMap :: (Int, Int) -> Map (Int, Int) Bool
gridMap (m, n) = M.fromList $ grid (m, n)

--findPaths' :: (Num t, Num t1, Ord t, Ord t1) =>
--    (t, t1) -> Map (t, t1) Bool -> [[(t, t1)]]
findPaths':: (Int, Int) -> Map (Int, Int) Bool -> [Int] -> ([[(Int, Int)]], [Int])
findPaths' (m, n) mp dice = go (1, 1) mp [] [] 0 [] 0 dice where
    go _ mpn _ _ _ _ _ _
        | mpn == M.empty = ([[]], [])
    go (0, 0) _ res _ _ dcSumL _ _ = (res, dcSumL) 
    go (x, y) mpn res path dc dcSumL sum dic
        | M.lookup (x + 1, y) mpn == Just True && x /= m = 
            go (x + 1, y) mpn res (path ++ [(x, y)]) dc dcSumL (sum + dicT) dicR 
        | M.lookup (x + 1, y) mpn == Just False && y /= n = 
            go (x, y + 1) setMapFalse res (path ++ [(x, y)]) dc dcSumL (sum + dicT) dicD
        -- only on first move down (dc == 0) grid is set to false in last column
        | x == m && y /= n && dc == 0 || (x == m && x == 1 && y /= n) = 
            go (x, y + 1) setMapFalse res (path ++ [(x, y)]) 1 dcSumL (sum + dicT) dicD
        | x == m && y /= n && dc == 1 = 
            go (x, y + 1) mpn res (path ++ [(x, y)]) dc dcSumL (sum + dicT) dicD
        -- end of finding one path
        | M.lookup (1, (n - 1)) mpn == Just True && x == m && y == n =
            go (1, 1) mpn (res ++ [(path ++ [(x, y)])]) [] 0 (dcSumL ++ [sum + dicT]) 0 dice
        -- end of finding all paths
        | M.lookup (1, (n - 1)) mpn == Just False && x == m && y == n =
            go (0, 0) mpn (res ++ [(path ++ [(x, y)])]) path 0 (dcSumL ++ [sum + dicT]) 0 dice
        | y == 1 && y == n && x == m =
            go (0, 0) mpn (res ++ [(path ++ [(x, y)])]) [] 0 (dcSumL ++ [sum + dicT]) 0 dice
--        | x == 1 && x == m && y == n =
--            go (0, 0) mpn (res ++ [(path ++ [(x, y)])]) [] 0 (dcSumL ++ [sum + dicT]) 0 dice
--        | M.lookup (x, y) mpn == Nothing =
--            go (0, 0) mpn (res ++ [(path ++ [(x, y)])]) path 0 (dcSumL ++ [sum + dicT]) 0 dice
        where
            setMapFalse = M.adjust (== False) (x, y) mpn
            dicT = head dic
            dicR = diceToR dic
            dicRT = head dicR
            dicD = diceDwn dic
            dicDT = head dicD

findPaths :: (Int, Int) -> ([[(Int, Int)]], [Int])
findPaths (m, n) = findPaths' (m, n) (gridMap (m, n)) dice
result (m, n)= maximum $ snd $ findPaths (m, n)

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


