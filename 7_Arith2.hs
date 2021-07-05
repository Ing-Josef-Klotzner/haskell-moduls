-- 7_Arith2.hs
-- arith2.hs
module Arith2 where
add :: Int -> Int -> Int
add x y = x + y
addPF :: Int -> Int -> Int
addPF = (+)
addOne :: Int -> Int
addOne = \x -> x + 1
addOnePF :: Int -> Int
addOnePF = (+1)

-- this is redefinition of print (already in Prelude)
print' :: Show a => a -> IO ()
print' = putStrLn . show

main :: IO ()
main = do
print "this is how print is defined (already in Prelude)"
print "print :: Show a => a -> IO ()"
print "print = putStrLn . show"
print "print (0 :: Int)"
print (0 :: Int)
print "print (add 1 0)"
print (add 1 0)
print "print (addOne 0)"
print (addOne 0)
print "print (addOnePF 0)"
print (addOnePF 0)
print "print ((addOne . addOne) 0)"
print ((addOne . addOne) 0)
print "print ((addOnePF . addOne) 0)"
print ((addOnePF . addOne) 0)
print "print ((addOne . addOnePF) 0)"
print ((addOne . addOnePF) 0)
print "print ((addOnePF . addOnePF) 0)"
print ((addOnePF . addOnePF) 0)
print "print (negate (addOne 0))"
print (negate (addOne 0))
print "print ((negate . addOne) 0)"
print ((negate . addOne) 0)
print "print ((addOne . addOne . addOne . negate . addOne) 0)"
print ((addOne . addOne . addOne . negate . addOne) 0)
