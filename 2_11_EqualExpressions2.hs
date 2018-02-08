-- 2_11_EqualExpressions2.hs
-- the validation of input is now real validation not compare to hardcode
module EqualExpressions where
import Control.Monad
--import Data.Char
--                "\n1 + 1 = " ++ show (1 + 1) ++
--                "\n2 = " ++ show (2)

data Challenge = Challenge { lhs :: String
                           , rhs :: String
                           , result_lhs :: String
                           , result_rhs :: String
                           }
type Collection = [Challenge]
xs :: Collection
xs = [ Challenge "1 + 1" "2" (show (1+1)) (show 2)
     , Challenge "10^2" "10 + 9 * 10" (show (10^2)) (show (10 + 9 * 10))
     , Challenge "400 - 37" "(-) 37 400" (show (400 - 37)) (show ((-) 37 400))
     , Challenge "100 `div` 3" "100 / 3" (show (100 `div` 3)) (show (100 / 3))
     , Challenge "2 * 5 + 18" "2 * (5 + 18)" (show (2 * 5 + 18)) (show (2 * (5 + 18)))
     ]
     
answerToBool :: Char -> Bool
answerToBool 'y' = True
answerToBool 'n' = False

main = do
    forM_ xs $ \s -> do
        putStrLn "\ntell me, if following expressions are equal"
        putStrLn $ show (lhs s)
        putStrLn $ show (rhs s)
        putStrLn "press y or n"
        c <- getChar
        putStrLn $ "\n" ++ show (lhs s) ++ " = " ++ show (result_lhs s)
        putStrLn $ show (rhs s) ++ " = " ++ show (result_rhs s)
        if answerToBool c == (result_lhs s == result_rhs s)
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
