module Main where
import Control.Monad (forM_, liftM)
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert el Leaf  = Node Leaf el Leaf
treeInsert el (Node left a right)
       | el == a = Node left el right
       | el < a = Node (treeInsert el left) a right
       | el > a = Node left a (treeInsert el right)

treeFromList :: Ord a => [a] -> BinaryTree a
treeFromList list = foldr treeInsert Leaf $ reverse list

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = x ++ y ++ z
  where
    x = [a]
    y = preorder left
    z = preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = x ++ y ++ z
  where
    x = preorder left
    y = [a]
    z = preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = x ++ y ++ z
  where
    x = postorder left
    y = postorder right
    z = [a]

-- iL ... input list  t ... tree
testpreorder iL = 
    if preorder tfL == iL
        then putStrLn "YES"
        else putStrLn "NO"
    where
    -- tree from list iL
    tfL = treeFromList iL

readLst :: Read a => IO [a]
readLst = liftM (map read . words) getLine

main :: IO ()
main = do
    [t] <- readLst :: IO [Int]
    forM_ [1..t] $ \tx -> do
        [n] <- readLst :: IO [Int]
        x <- readLst :: IO [Int]
--        putStrLn $ show x
        testpreorder x 
--  testPreorder
--  testInorder
--  testPostorder
