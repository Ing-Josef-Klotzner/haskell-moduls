-- 9_9_Exercises.hs
module Exercises_9_9 where

itIsMystery xs = map (\x -> elem x "aeiou") xs

question1 = "\nWill the following expressions return a value or be ⊥ (means bottom)?"
question2 = "Is it in normal form?"
question3 = ""
question4 = "What will be the result?"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 1 $ map (+1) [undefined, 2, 3]\n" ++
                        "a) returns a value\n" ++
                        "b) reaches bottom"
                 , question = question1
                 , result_lhs = "b) reaches bottom (*** Exception: Prelude.undefined)"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "take 1 $ map (+1) [1, undefined, 3]\n" ++
                        "a) reaches bottom\n" ++
                        "b) returns a value"
                 , question = question1
                 , result_lhs = "b) returns a value ( [2] )"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What does the following mystery function do? What is its\n" ++
                        "type? Describe it (to yourself or a loved one) in standard\n" ++
                        "English and then test it out in the REPL to make sure you\n" ++
                        "were correct (function is already loaded)\n" ++
                        "itIsMystery xs = map (\x -> elem x "aeiou") xs\n" ++
                        "a) it is mystery\n" ++
                        "b) it maps first element to rest of list, if it is element of 'aeiou'\n" ++
                        "c) it maps 'aeiou' to each element of the list\n" ++
                        "d) it returns a list telling in bool language, if element of input list contains 'aeiou'" 
                 , question = question3
                 , result_lhs = "d) it returns a list telling in bool language, if element of input list contains 'aeiou'"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "map (^2) [1..10]\n" ++
                        "a) [2,4,6,8,10,12,14,16,18,20]\n" ++
                        "b) [1,4,9,16,25,36,49,64,81,100]\n" ++
                        "c) [1,0,-1,-2,-3,-4,-5,-6,-7,-8]\n" ++
                        "d) [3,4,5,6,7,8,9,10,11,12]\n"
                 , question = question4
                 , result_lhs = "b) [1,4,9,16,25,36,49,64,81,100]"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "map minimum [[1..10], [10..20], [20..30]]\n" ++
                        "a) [1,10,20]\n" ++
                        "b) [1,0,0]\n" ++
                        "c) [10,10,10]\n" ++
                        "d) [0,10,20]\n"
                 , question = question4
                 , result_lhs = "a) [1,10,20]"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "map sum [[1..5], [1..5], [1..5]]" ++
                        "a) [45]\n" ++
                        "b) [15,15,15]\n" ++
                        "c) [6,6,6]\n" ++
                        "d) [3,6,9,12,15]\n"
                 , question = question4
                 , result_lhs = "b) [15,15,15]"
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
