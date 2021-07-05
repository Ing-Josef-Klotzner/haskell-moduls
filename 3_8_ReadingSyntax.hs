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
xs = [ Challenge { lhs = "concat [[1, 2, 3], [4, 5, 6]]"
                 , result_lhs = "[1,2,3,4,5,6]"
                 , yes_no = 'y'  -- verified with ghci
                 }
     , Challenge { lhs = "++ [1, 2, 3] [4, 5, 6]"
                 , result_lhs = "parse error on input `++'" ++
                                "\n\nthis is correct: (++) [1, 2, 3] [4, 5, 6]" ++
                                "\nresult when correct: [1,2,3,4,5,6]"
                 , yes_no = 'n'
                 }
     , Challenge { lhs = "(++) \"hello\" \" world\""
                 , result_lhs = "\"hello world\""
                 , yes_no = 'y'
                 }
     , Challenge { lhs = "[\"hello\" ++ \" world]"
                 , result_lhs = "lexical error in string/character literal at end of input" ++
                                "\n\nthis is correct: [\"hello\" ++ \" world\"]" ++
                                "\nresult when correct: [\"hello world\"]"
                 , yes_no = 'n'
                 }
     , Challenge { lhs = "4 !! \"hello\""
                 , result_lhs = "Couldn't match expected type `Int' with actual type `[Char]'" ++
                                "\nIn the second argument of `(!!)', namely `\"hello\"'" ++
                                "\nIn the expression: 4 !! \"hello\"" ++
                                "\nIn an equation for `it': it = 4 !! \"hello\"" ++
                                "\n\nthis is correct: \"hello\" !! 4" ++
                                "\nresult when correct: 'o'"
                 , yes_no = 'n'
                 }
     , Challenge { lhs = "(!!) \"hello\" 4"
                 , result_lhs = "'o'"
                 , yes_no = 'y'
                 }
     , Challenge { lhs = "take \"4 lovely\""
                 , result_lhs = "Couldn't match expected type `Int' with actual type `[Char]'" ++
                                "\nIn the first argument of `take', namely `\"4 lovely\"'" ++
                                "\nIn the expression: take \"4 lovely\"" ++
                                "\nIn an equation for `it': it = take \"4 lovely\"" ++
                                "\n\nthis is correct: take 4 \"lovely\"" ++
                                "\nresult when correct: \"love\""
                 , yes_no = 'n'
                 }
     , Challenge { lhs = "take 3 \"awesome\""
                 , result_lhs = "\"awe\""
                 , yes_no = 'y'
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ "\nIs this code written correctly? (y/n) "
                         , ""
                         , lhs c
                         , ""
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
