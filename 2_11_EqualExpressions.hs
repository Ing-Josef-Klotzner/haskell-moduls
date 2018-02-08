-- 2_11_EqualExpressions.hs
module EqualExpressions where
import Control.Monad
--import Data.Char
--                "\n1 + 1 = " ++ show (1 + 1) ++
--                "\n2 = " ++ show (2)

data Challenge = Challenge { lhs :: String
                           , rhs :: String
                           , result_lhs :: String
                           , result_rhs :: String
                           , yes_no :: Char
                           }
type Collection = [Challenge]
xs :: Collection
xs = [ Challenge "1 + 1" "2" (show (1+1)) (show 2) 'y'
     , Challenge "10^2" "10 + 9 * 10" (show (10^2)) (show (10 + 9 * 10)) 'y'
     , Challenge "400 - 37" "(-) 37 400" (show (400 - 37)) (show ((-) 37 400)) 'n'
     , Challenge "100 `div` 3" "100 / 3" (show (100 `div` 3)) (show (100 / 3)) 'n'
     , Challenge "2 * 5 + 18" "2 * (5 + 18)" (show (2 * 5 + 18)) (show (2 * (5 + 18))) 'n'
     ]
main = do
    forM_ xs $ \s -> do
        putStrLn "\ntell me, if following expressions are equal"
        putStrLn $ show (lhs s)
        putStrLn $ show (rhs s)
        putStrLn "press y or n"
        c <- getChar
        putStrLn $ "\n" ++ show (lhs s) ++ " = " ++ show (result_lhs s)
        putStrLn $ show (rhs s) ++ " = " ++ show (result_rhs s)
        if c == yes_no s
        then
            putStrLn "\nYou are right!"
        else
            putStrLn "\nNo! What a pitty!"
-- 1. 1 + 1
-- 2
-- 2. 10 ^ 2
-- 10 + 9 * 10
-- 3. 400 - 37
-- (-) 37 400
-- 4. 100 `div` 3
-- 100 / 3
-- 5. 2 * 5 + 18
-- 2 * (5 + 18)
