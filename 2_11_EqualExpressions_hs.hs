-- 2_11_EqualExpressions.hs
module EqualExpressions where
import Control.Monad
import Control.Exception
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
xs = [ Challenge { lhs = "1 + 1"
                 , rhs = "2"
                 , result_lhs = show (1+1)
                 , result_rhs = show 2
                 }
     , Challenge { lhs = "10^2"
                 , rhs = "10 + 9 * 100"
                 , result_lhs = show (10^2)
                 , result_rhs = show (10 + 9 * 10)
                 }
     , Challenge { lhs = "400 - 37"
                 , rhs = "(-) 37 400"
                 , result_lhs = show (400 - 37)
                 , result_rhs = show ((-) 37 400)
                 }
     , Challenge { lhs = "100 `div` 3"
                 , rhs = "100 / 3"
                 , result_lhs = show (100 `div` 3)
                 , result_rhs = show (100 / 3)
                 }
     , Challenge { lhs = "2 * 5 + 18"
                 , rhs = "2 * (5 + 18)"
                 , result_lhs = show (2 * 5 + 18)
                 , result_rhs = show (2 * (5 + 18))
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ "Is this equation: "
                         , ""
                         , lhs c ++ " = " ++ rhs c
                         , ""
                         ] ++ "correct? "

data Answer = Yes | No deriving (Show, Read)

isCorrect :: Challenge -> Bool
isCorrect c = result_lhs c == result_rhs c

verifyAnswer :: Challenge -> Answer -> Bool
verifyAnswer c a = isCorrect c == answerToBool a
  where
    answerToBool No  = False
    answerToBool Yes = True

showResult :: Challenge -> Answer -> String
showResult c a = unlines [ result_lhs c ++ comparison ++ result_rhs c
                         , ""
                         , "Your answer is " ++ userResult
                         , ""
                         , "-----------------------------------"
                         , ""
                         ]
  where
    comparison = if isCorrect c
                 then " = "
                 else " /= "
    userResult = if verifyAnswer c a
                 then "correct, congratulations!"
                 else "sadly, completely and utterly wrong!"

safeReadLn :: Read a => IO a
safeReadLn = readLn `catch` askAgain
  where
    askAgain :: Read a => SomeException -> IO a
    askAgain _ = putStr "Sorry, I did not get that. Please try again: " >> safeReadLn

execChallenge :: Challenge -> IO Bool
execChallenge c = do
  putStr $ askChallenge c
  answer <- safeReadLn
  putStrLn ""
  putStrLn $ showResult c answer
  return $ verifyAnswer c answer

main = do
  rs <- mapM execChallenge xs
  let points = length . filter id $ rs
  let possiblePoints = length xs
  putStrLn $ "Congratulations, scored " ++ show points ++ "/" ++ show possiblePoints ++ " (" ++ show (fromIntegral 100 * fromIntegral points/ fromIntegral possiblePoints) ++ "%)" ++ "!"
