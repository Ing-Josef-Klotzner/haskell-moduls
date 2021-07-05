-- 16_Exercises_functor.hs
module Exercises_functor where
import Control.Monad (void)
--Exercises: Heavy Lifting
--Add fmap, parentheses, and function composition to the expres-
--sion as needed for the expression to typecheck and produce
--the expected result. It may not always need to go in the same
--place, so don’t get complacent.

-- 1.
a = fmap (+1) $ read "[1]" :: [Int]

--Expected result
--Prelude> a
--[2]

-- 2.
b = tw_fmap (++ "lol") (Just ["Hi,", "Hello"])
tw_fmap = fmap . fmap
--Prelude> b
--Just ["Hi,lol","Hellolol"]

--3.
c = fmap (*2) (\x -> x - 2)
-- Prelude> c 1
-- -2

--4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
--Prelude> d 0
--"1[0,1,2,3]"

--5.
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
--        changed = fmap (fmap read ("123"++)) (fmap show ioi)
--  is the same as:
        changed = read . ("123" ++) . show <$> ioi
    in  (*3) <$> changed
--Prelude> e
--3693

main :: IO ()
main = do
    putStrLn $ "\n *>fmap (+1) $ read \"[1]\" :: [Int]\n" ++ show a
    putStrLn "Expected result:   [2]\n"

    putStrLn $ " *>(fmap . fmap) (++ \"lol\") (Just [\"Hi,\", \"Hello\"])\n" ++ show b
    putStrLn "Expected result:   Just [\"Hi,lol\",\"Hellolol\"]\n"

    putStrLn $ " *>fmap (*2) (\\x -> x - 2) $ 1\n" ++ show (c 1)
    putStrLn "Expected result:   -2\n"

    putStrLn $ " *>fmap ((return '1' ++) . show) (\\x -> [x, 1..3])\n" ++ show (d 0)
    putStrLn "Expected result:   \"1[0,1,2,3]\"\n"
    
    putStrLn $ "e :: IO Integer\n" ++ 
            "e = let ioi = readIO \"1\" :: IO Integer\n" ++
            "        changed = read . (\"123\" ++) . show <$> ioi\n" ++
            "    in  (*3) <$> changed\n\n" ++
            " *>e"
    out <- e            
    putStrLn $ show out
    putStrLn "Expected result:    3693\n"

