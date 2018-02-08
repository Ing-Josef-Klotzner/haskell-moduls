-- 4_6_GoOnBoolMe.hs
module GoOnBoolMe where

data Challenge = Challenge { lhs :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
--1. not True && true
--2. not (x = 6)
--3. (1 * 2) > 5
--4. [Merry] > [Happy]
--5. [1, 2, 3] ++ "look at me!"
xs :: Collection
xs = [ Challenge { lhs = "not True && true"
                 , result_lhs = "Not in scope: `true'" ++
                                "\n\nthis is correct: not True && True" ++
                                "\nresult when correct: False"
                 , yes_no = 'n'  -- verified with ghci
                 }
     , Challenge { lhs = "not (x = 6)"
                 , result_lhs = "parse error on input `='" ++
                                "\n\nthis is correct: not (x == 6)" ++
                                "\nresult when correct and x set to 6 before: False" ++
                                "\nresult when correct and x set to other int than 6 before: True"
                 , yes_no = 'n'
                 }
     , Challenge { lhs = "(1 * 2) > 5"
                 , result_lhs = "False"
                 , yes_no = 'y'
                 }
     , Challenge { lhs = "[Merry] > [Happy]"
                 , result_lhs = "Not in scope: data constructor `Merry'" ++
                                "Not in scope: data constructor `Happy'" ++
                                "\n\nthis is correct: [\"Merry\"] > [\"Happy\"]" ++
                                "\nresult when correct: True"
                 , yes_no = 'n'
                 }
     , Challenge { lhs = "[1, 2, 3] ++ \"look at me!\""
                 , result_lhs = "No instance for (Num Char) arising from the literal `1'" ++
                                "\nPossible fix: add an instance declaration for (Num Char)" ++
                                "\nIn the expression: 1" ++
                                "\nIn the first argument of `(++)', namely `[1, 2, 3]'" ++
                                "\nIn the expression: [1, 2, 3] ++ \"look at me!\"" ++
                                "\n\nthis is correct: \"[1, 2, 3]\" ++ \"look at me!\"" ++
                                "\nresult when correct: \"[1, 2, 3]look at me!\""
                 , yes_no = 'n'
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ "\nIs this Bool Expression written correctly? (y/n) "
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
