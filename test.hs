module Test where
import PrintBinaryTree

print_node = printBinaryTree $ insert' 1 $ insert' 3 $ insert' (-3) $ insert' (-2) $ insert' (-1) $ insert' 4 $ insert' 2 $ treeBuild 1

node1 = Leaf
