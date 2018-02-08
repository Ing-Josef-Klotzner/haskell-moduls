module PrintBinaryTree (printBinaryTree, treeBuild, unfold, filename, rtFSz, rootX, rootY, 
                        graph_tree, draw_tree, insert', mapTree, BinaryTree(..)) where
import Data.Char

-- Inspired from the Book "Haskell Programming from first prinicples" (Christopher Allen, Julie Mononuki)
-- (Pure functional programming without fear or frustration)

-- created by Ing. Josef Klotzner
-- function 'printBinaryTree binTree' writes a svg file ./BinaryTreePrint.svg with svg graphics
-- from a BinaryTree binTree
-- to see graphics file drag it to a browser or open it with a program being able to handle svg format

-- Purpose: For everybody who wants to visualize his BinaryTree(s)

--1. Write unfold for BinaryTree.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = go (f x) where
    go (Just (y,b,z)) = Node (unfold f y) b (unfold f z)
    go Nothing = Leaf
-- *ChapterExercises_12_5> unfold (\x -> if x > 0 then (Just(x-1,x,x-1)) else Nothing) 3
--Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 1 Leaf)) 3 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 1 Leaf))

--2. Make a tree builder.
--Using the unfold function you’ve made for BinaryTree, write
--the following function:
treeBuild :: (Num b, Ord b) => b -> BinaryTree b
--treeBuild o@n = unfold (\x -> if x < o then (Just(x + 1, x, x + 1)) else Nothing) (o - n)
treeBuild n = unfold (\x -> if x < n then (Just(x + 1, x, x + 1)) else Nothing) 0
--You should be producing results that look like the following:
--Prelude> treeBuild 0
--Leaf
--Prelude> treeBuild 1
--Node Leaf 0 Leaf
--Prelude> treeBuild 2
--Node (Node Leaf 1 Leaf)
--      0
--      (Node Leaf 1 Leaf)
--Prelude> treeBuild 3
--Node  (Node (Node Leaf 2 Leaf))
--            1
--            (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
--Or in a slightly different representation:
--     0
--     0
--   /   \
--   1   1
--   0
-- /   \
-- 1   1
-- /\  /\
-- 2 2 2 2

-- <?xml version="1.0" encoding="UTF-8" standalone="no"?>
-- <svg width="210mm" height="297mm" version="1.1"
--    xmlns="http://www.w3.org/2000/svg">
--    <desc>Just a 0 node for test</desc>
--    <text style="font-size:160px;fill:#000000;font-family:Sans" x="340" y="140">1</text>
--    <text style="font-size:160px;fill:#000000;font-family:Sans" x="280" y="300">/</text>
--    <text style="font-size:160px;fill:#000000;font-family:Sans" x="448" y="300">\</text>
-- </svg>
createdBy = "Created by Josef Klotzner (printBinaryTree, Haskell, svg)"
string1 = map chr [65..126]
string2 = map chr [160..250]
svg_head = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++
            "<!-- Created by printBinaryTree (Haskell, Josef Klotzner) -->\n" ++
            "<svg width=\"210mm\" height=\"297mm\" version=\"1.1\"\n" ++
            "   xmlns=\"http://www.w3.org/2000/svg\">\n" ++
            "   <text style=\"font-size:8px;fill:#000000;font-family:Sans\" \n" ++ 
            "   x=\"40\" y=\"40\">" ++ createdBy ++ "</text>\n"
-- only to show various characters of ascii table
svg_head1 = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++
            "<!-- Created by printBinaryTree (Haskell, Josef Klotzner) -->\n" ++
            "<svg width=\"210mm\" height=\"297mm\" version=\"1.1\"\n" ++
            "   xmlns=\"http://www.w3.org/2000/svg\">\n" ++
            "   <text style=\"font-size:8px;fill:#000000;font-family:Sans\" \n" ++ 
            "   x=\"40\" y=\"40\">" ++ string1 ++ "</text>\n" ++
            "   <text style=\"font-size:16px;fill:#000000;font-family:Sans\" \n" ++ 
            "   x=\"40\" y=\"80\">" ++ string2 ++ "</text>\n"
svg_char :: (Show a, Show a1, Show a2) => a -> a1 -> a2 -> [Char] -> [Char]
svg_char fSz x y c = "   <text style=\"font-size:" ++ show fSz ++ "px;fill:#000000;font-family:Sans\"" ++
            " x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\">" ++ c ++ "</text>\n"
svg_node :: (Fractional a, Show a) => a -> a -> a -> [Char] -> [Char]
svg_node fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\")
-- a node plus next node root positions with leafs
--     x
--    / \
--   x   x
svg_node_plus fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\") ++
                    (svg_char (fSz / 2) (x - 80 / 160 * fSz) (y + fSz * 1.6) "x") ++
                    (svg_char (fSz / 2) (x + 130 / 160 * fSz) (y + fSz * 1.6) "x")
svg_node_ll fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\") ++
                    (svg_char (fSz / 2) (x - 80 / 160 * fSz) (y + fSz * 1.6) "x")
svg_node_rl fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\") ++
                    (svg_char (fSz / 2) (x + 130 / 160 * fSz) (y + fSz * 1.6) "x")
svg_leaf fSz x y = (svg_char fSz x y "x") 
svg_tail = "</svg>"
filename = "./BinaryTreePrint.svg"
filename1 = "./test.svg"
rtFSz = 160
rootX = 340
rootY = 140
rootLvl = 0
--write_svg = writeFile filename1 (svg_head ++ (svg_node rtFSz rootX rootY "0") ++ svg_tail)
write_svg = writeFile filename1 (svg_head1 ++ (svg_node rtFSz rootX rootY "") ++ svg_tail)
printBinaryTree :: Show a => BinaryTree a -> IO ()
printBinaryTree tree = writeFile filename (svg_head ++ 
                draw_tree (graph_tree rtFSz rootX rootY rootLvl tree) ++ svg_tail)
-- test to show the leafs:
--  printBinaryTree $ insert' 1 $ insert' 3 $ insert' (-3) $ insert' (-2) $ insert' (-1) $ insert' 4 $ insert' 2 $ treeBuild 1
--Good work.

graph_tree :: (Fractional t, Num t1) => t -> t -> t -> t1 -> BinaryTree t2 -> BinaryTree (t, t, t, t1, t2)
graph_tree fSz x y tLvl Leaf = Leaf
graph_tree fSz x y tLvl (Node Leaf a Leaf) = Node Leaf (fSz, x, y, tLvl, a) Leaf
graph_tree fSz x y tLvl (Node left a Leaf) = Node (graph_tree (fSz / 2) (x - 80 / 160 * fSz)
                                            (y + fSz * 1.6) (tLvl + 1) left) (fSz, x, y, tLvl, a) Leaf
graph_tree fSz x y tLvl (Node Leaf a right) = Node Leaf (fSz, x, y, tLvl, a) (graph_tree (fSz / 2) 
                                            (x + 130 / 160 * fSz) (y + fSz * 1.6) (tLvl + 1) right)
graph_tree fSz x y tLvl (Node left a right) = Node (graph_tree (fSz / 2) (x - 80 / 160 * fSz)
                                            (y + fSz * 1.6) (tLvl + 1) left) (fSz, x, y, tLvl, a) 
                                            (graph_tree (fSz / 2) 
                                            (x + 130 / 160 * fSz) (y + fSz * 1.6) (tLvl + 1) right)
node = treeBuild 7
node1 = insert' 7 node
node2 = insert' 8 node1
node3 = insert' 9 node2
test = printBinaryTree node3

draw_tree :: (Fractional a, Show a, Show a1) => BinaryTree (a, a, a, t, a1) -> [Char]
draw_tree Leaf = svg_leaf rtFSz rootX rootY
draw_tree (Node Leaf (fSz, x, y, tLvl, a) Leaf) = svg_node_plus fSz x y (show a)
draw_tree (Node left (fSz, x, y, tLvl, a) Leaf) = draw_tree left ++ svg_node_rl fSz x y (show a)
draw_tree (Node Leaf (fSz, x, y, tLvl, a) right) = svg_node_ll fSz x y (show a) ++ draw_tree right
draw_tree (Node left (fSz, x, y, tLvl, a) right) = draw_tree left ++ svg_node fSz x y (show a) ++ draw_tree right



insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

