-- typeInference1.hs
module TypeInference1 where
f :: Num a => a -> a -> a
f x y = x + y + 3

-- Type signature of general function
-- (++) :: [a] -> [a] -> [a]
-- How might that change when we apply
-- it to the following value?
myConcat x = x ++ " yo"
-- General function
-- (*) :: Num a => a -> a -> a
-- (/) :: Fractional a => a -> a -> a
-- Applied to a value
myMult x = (x / 3) * 5
-- take :: Int -> [a] -> [a]
myTake x = take x "hey you"
-- (>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10])
-- (<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z'

main = do
    putStrLn $ "Type signature of general function" ++
            "\n(++) :: [a] -> [a] -> [a]" ++
            "\nleads to" ++
            "\nmyConcat :: [Char] -> [Char]" ++
            "\nwhen used in function" ++
            "\nmyConcat x = x ++ \" yo\"" ++
            "\nit is loaded - play with it\n" ++

            "\nType signature of general functions" ++
            "\n(*) :: Num a => a -> a -> a" ++
            "\n(/) :: Fractional a => a -> a -> a" ++
            "\nleads to" ++
            "\nmyMult :: Fractional a => a -> a" ++
            "\nwhen used in function" ++
            "\nmyMult x = (x / 3) * 5\n" ++

            "\nType signature of general function" ++
            "\ntake :: Int -> [a] -> [a]" ++
            "\nleads to" ++
            "\nmyTake :: Int -> [Char]" ++
            "\nwhen used in function" ++
            "\nmyTake x = take x \"hey you\"\n" ++

            "\nType signature of general function" ++
            "\n(>) :: Ord a => a -> a -> Bool" ++
            "\nleads to" ++
            "\nmyCom :: Int -> Bool" ++
            "\nwhen used in function" ++
            "\nmyCom x = x > (length [1..10])\n" ++

            "\nType signature of general function" ++
            "\n(<) :: Ord a => a -> a -> Bool" ++
            "\nleads to" ++
            "\nmyAlph :: Char -> Bool" ++
            "\nwhen used in function" ++
            "\nmyAlph x = x < 'z'"
