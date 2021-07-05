-- 9_8_Exercises.hs
module Exercises_9_8 where

question1 = "\nWill the following expressions return a value or be ⊥ (means bottom)?"
question2 = "Is it in normal form?"
question3 = ""

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "[x^y | x <- [1..5], y <- [2, undefined]]\n" ++
                        "a) returns a value\n" ++
                        "b) reaches bottom"
                 , question = question1
                 , result_lhs = "b) reaches bottom (*** Exception: Prelude.undefined)"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "b) returns a value ( [1] )"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "sum [1, undefined, 3]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "a) reaches bottom (*** Exception: Prelude.undefined)"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "length [1, 2, undefined]]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "b) returns a value ( 3 )"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "length $ [1, 2, 3] ++ undefined\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "a) reaches bottom (*** Exception: Prelude.undefined)"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 1 $ filter even [1, 2, 3, undefined]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "b) returns a value ( [2] )"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 1 $ filter even [1, 3, undefined]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "a) reaches bottom (*** Exception: Prelude.undefined)"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 1 $ filter odd [1, 3, undefined]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "b) returns a value ( [1] )"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 2 $ filter odd [1, 3, undefined]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "b) returns a value ( [1,3] )"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 3 $ filter odd [1, 3, undefined]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "a) reaches bottom ( [1,3*** Exception: Prelude.undefined )"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "[1, 2, 3, 4, 5]\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "a) normal form, which implies weak head normal form"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "1 : 2 : 3 : 4 : _\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "c) neither"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "enumFromTo 1 10\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "b) weak head normal form only"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "length [1, 2, 3, 4, 5]\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "b) weak head normal form only"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "sum (enumFromTo 1 10)\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "b) weak head normal form only"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "['a'..'m'] ++ ['n'..'z']\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "b) weak head normal form only"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "(_, 'b')\n" ++
                        "a) normal form, which implies weak head normal form\n" ++
                        "b) weak head normal form only\n" ++
                        "c) neither"
                 , question = question2
                 , result_lhs = "c) neither"
                 , yes_no = "c"
                 }
     ]
askChallenge :: Challenge -> String
askChallenge c = unlines [ question c
                         , ""
                         , lhs c
                         , ""
                         ]

type Answer = String

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

execChallenge :: Challenge -> IO Bool
execChallenge c = do
  putStr $ askChallenge c
  answer <- getLine
  putStrLn ""
  putStrLn $ showResult c answer
  return $ verifyAnswer c answer

main = do
    rs <- mapM execChallenge xs
    let points = length . filter id $ rs
    let possiblePoints = length xs
    putStrLn $ "Congratulations, scored " ++ show points ++ "/" ++ show possiblePoints ++ " (" ++ 
        show (fromIntegral 100 * fromIntegral points/ fromIntegral possiblePoints) ++ "%)" ++ "!"
