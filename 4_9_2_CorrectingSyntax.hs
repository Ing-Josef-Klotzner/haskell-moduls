-- 4_9_2_CorrectingSyntax.hs
module CorrectingSyntax where

x = (+)
f :: [a] -> Int
f xs = w `x` 1
    where w = length xs

s (a, b) = a
    
main = do
    putStrLn $ "x = (+)" ++
                "\nf :: [a] -> Int" ++
                "\nf xs = w `x` 1" ++
                "\n\napplied to [1,2,3,4] results in " ++ show (f [1,2,3,4])
    -- identity function
    putStrLn $ "\n(\\x -> x) 8 is same as id 8 - result: " ++ show ((\x -> x) 8)
    putStrLn $ "\ns (a, b) = a applied to (6,17) - result: " ++ show (s (6,17))
