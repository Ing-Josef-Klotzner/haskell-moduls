-- 2_11_Paranthesization.hs
module Paranthesization where

main = putStrLn $ "\n2 + 2 * 3 - 1 = " ++ show (2 + 2 * 3 - 1) ++
                "\n2 + (2 * 3) - 1 = " ++ show (2 + (2 * 3) - 1) ++
                
                "\n\n(^) 10 $ 1 + 1 = " ++ show ((^) 10 $ 1 + 1) ++
                "\n(^) 10 (1 + 1) = " ++ show ((^) 10 (1 + 1)) ++
                
                "\n\n2 ^ 2 * 4 ^ 5 + 1 = " ++ show (2 ^ 2 * 4 ^ 5 + 1) ++
                "\n((2 ^ 2) * (4 ^ 5)) + 1 = " ++ show (((2 ^ 2) * (4 ^ 5)) + 1)

