-- 3_8_1_ReadingSyntax.hs
module ReadingSyntax where
import Control.Monad
import Control.Exception

data Challenge = Challenge { lhs :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]

xs :: Collection
xs = [ Challenge { lhs = "a) concat [[1 * 6], [2 * 6], [3 * 6]]"
                 , result_lhs = "d) [6,12,18]"
                 , yes_no = 'd'
                 }
     , Challenge { lhs = "b) \"rain\" ++ drop 2 \"elbow\""
                 , result_lhs = "c) \"rainbow\""
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "c) 10 * head [1, 2, 3]"
                 , result_lhs = "e) 10"
                 , yes_no = 'e'
                 }
     , Challenge { lhs = "d) (take 3 \"Julie\") ++ (tail \"yes\")"
                 , result_lhs = "a) \"Jules\""
                 , yes_no = 'a'
                 }
     , Challenge { lhs = "e) concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]"
                 , result_lhs = "b) [2,3,5,6,8,9]"
                 , yes_no = 'b'
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ "\nexpression: " ++ lhs c
                         , "\nWhich is correct result? (a-e): "
                         , "a) \"Jules\""
                         , "b) [2,3,5,6,8,9]"
                         , "c) \"rainbow\""
                         , "d) [6,12,18]"
                         , "e) 10"
                         ]

type Answer = Char

verifyAnswer :: Challenge -> Answer -> Bool
verifyAnswer c a = yes_no c == a

showResult :: Challenge -> Answer -> String
showResult c a = unlines [ result_lhs c  -- a:[]
                         , ""
                         , "Your answer is " ++ userResult
                         , ""
                         , "-----------------------------------"
                         , ""
                         ]
  where
    userResult = if verifyAnswer c a
                 then "correct, congratulations!"
                 else "sadly, completely and utterly wrong!"

-- safeReadLn :: Read a => IO a
-- safeReadLn = getChar `catch` askAgain
--   where
--     askAgain :: Read a => SomeException -> IO a
--     askAgain _ = putStr "Sorry, I did not get that. Type 'y' or 'n'. Please try again: " >> safeReadLn

execChallenge :: Challenge -> IO Bool
execChallenge c = do
  putStr $ askChallenge c
  answer <- getChar --safeReadLn
  putStrLn ""
  putStrLn $ showResult c answer
  return $ verifyAnswer c answer

main = do
  rs <- mapM execChallenge xs
  let points = length . filter id $ rs
  let possiblePoints = length xs
  putStrLn $ "Congratulations, scored " ++ show points ++ "/" ++ show possiblePoints ++ " (" ++ 
        show (fromIntegral 100 * fromIntegral points/ fromIntegral possiblePoints) ++ "%)" ++ "!"
