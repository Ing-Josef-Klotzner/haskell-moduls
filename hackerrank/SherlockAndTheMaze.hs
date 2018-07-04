module Main where
import Data.List (intercalate)
import Control.Monad (forM)
import Data.Array

cntArr :: Array (Int, Int, Int) Integer
cntArr =
    listArray ((0, 0, 0), (9, 9, 1)) $
        [f n m o | n <- [0..9], m <- [0..9], o <- [0, 1]] where
        f 0 0 _ = 0
        f _ 0 0 = 1
        f 0 _ 1 = 1
        f n m 0
--            | k > 0 && n > 0 = cntArr ! (n - 1, m, k - 1, 1) + cntArr ! (n - 1, m, k, 0)
            | n > 0 = cntArr ! (n-1, m, 1)
            | otherwise = 0
        f n m 1
--            | k > 0 && m > 0 = cntArr ! (n, m - 1, k - 1, 0) + cntArr ! (n, m - 1, k, 1)
            | m > 0 = cntArr ! (n, m-1, 0)
            | otherwise = 0

cnt :: (Int, Int, Int) -> Integer
cnt (0, 0, _) = 1
cnt (n, m, k) = foldr f 0 [0 .. k] where
  f k' acc = (acc + cntArr ! (n, m, 0) + cntArr ! (n, m, 1)) `rem` (10^9 + 7)

main :: IO()
main = do
    t <- fmap (read :: String -> Int) getLine
    nkL <- forM [1..t] (\_ -> do fmap (map (read :: String -> Int).words) getLine)
    
    putStrLn $ intercalate "\n" $ map (\[n, m, k] -> show (cnt (n, m, k))) $ nkL

--A binary tree is a tree which is characterized by any of the following properties:

--It can be empty (null).
--It can contain a root node which contain some value and two subtree, left subtree and right subtree, which are also binary tree.
--A binary tree is a binary search tree (BST) if all the non-empty nodes follows both two properties:

--If node has a left subtree, then all the values in its left subtree are smaller than the value of the current node.
--If node has a right subtree, then all the value in its right subtree are greater than the value of the current node.
--You are given N nodes, each having unique value ranging from [1, N], how many different binary search tree can be created using all of them.

--Input 
--First line will contain an integer, T, number of test cases. Then T lines follow, where each line represent a test case. Each test case consists a single integer, N, where N is the number of nodes in the binary search tree.

--Output 
--For each test case, find the number of different binary search trees that can be created using these nodes. Print the answer modulo (108+7).

--Constraints 
--1 <= T <= 1000 
--1 <= N <= 1000

--Sample Input

--5
--1
--2
--3
--4
--100
--Sample Output

--1
--2
--5
--14
--25666077
--Explanation 
--Test Case #1: We have only one tree.

--1
--Test Case #2: Two trees can be created using two nodes.

--1          2
-- \        /
--  2      1
--Test Case #3:

--1          1         2         3        3
-- \          \       / \       /        /
--  2          3     1   3     1        2
--   \        /                 \      /
--    3      2                   2    1
