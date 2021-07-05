-- 10_UnderstandingFolds.hs
module UnderstandingFolds where

itIsMystery xs = map (\x -> elem x "aeiou") xs

question1 = "\nfoldr (*) 1 [1..5] will return the same result as which of the following:"
question2 = "One difference between foldr and foldl is:"
question3 = "Folds are catamorphisms, which means they are generally used to ..."
question4 = "Which fold is correct?"
question5 = "Will this ever return a different answer?"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "for multiple answer type f.e. 'ad' and enter\n" ++
                        "a) flip (*) 1 [1..5]\n" ++
                        "b) foldl (flip (*)) 1 [1..5]\n" ++
                        "c) foldl (*) 1 [1..5]\n" ++
                        "d) foldr [*] 1 [1..5]"
                 , question = question1
                 , result_lhs = "b) foldl (flip (*)) 1 [1..5]\n" ++
                                "c) foldl (*) 1 [1..5]"
                 , yes_no = "bc"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) foldr, but not foldl, traverses the spine of a list from right to left\n" ++
                        "b) foldr, but not foldl, always forces the rest of the fold\n" ++
                        "c) foldr, but not foldl, associates to the right\n" ++
                        "d) foldr, but not foldl, is recursive"
                 , question = question2
                 , result_lhs = "c) foldr, but not foldl, associates to the right"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) reduce structure\n" ++
                        "b) expand structure\n" ++
                        "c) render you catatonic\n" ++
                        "d) generate infinite data structures" 
                 , question = question3
                 , result_lhs = "a) reduce structure"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "for multiple answer type f.e. 'ad' and enter\n" ++
                        "a) foldr (++) [\"woot\", \"WOOT\", \"woot\"]\n" ++
                        "b) foldr (++) [] [\"woot\", \"WOOT\", \"woot\"]\n" ++
                        "c) foldr (++) \"\" [\"woot\", \"WOOT\", \"woot\"]\n" ++
                        "d) foldr (++) '' [\"woot\", \"WOOT\", \"woot\"]\n"
                 , question = question4
                 , result_lhs = "b) foldr (++) [] [\"woot\", \"WOOT\", \"woot\"]\n" ++
                                "c) foldr (++) \"\" [\"woot\", \"WOOT\", \"woot\"]"
                 , yes_no = "bc"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "\n" ++
                        "a) foldr max [] \"fear is the little death\"\n" ++
                        "b) foldr max ' ' \"fear is the little death\"\n" ++
                        "c) foldr max '' \"fear is the little death\"\n" ++
                        "d) foldr max \" \" \"fear is the little death\"\n"
                 , question = question4
                 , result_lhs = "b) foldr max ' ' \"fear is the little death"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "\n" ++
                        "a) foldr and True [False, True]\n" ++
                        "b) foldr && True [False, True]\n" ++
                        "c) foldr (and) True [False, True]\n" ++
                        "d) foldr (&&) True [False, True]\n"
                 , question = question4
                 , result_lhs = "d) foldr (&&) True [False, True]"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' and enter\n" ++
                        "foldr (||) True [False, True]\n" ++
                        "a) Yes\n" ++
                        "b) never"
                 , question = question5
                 , result_lhs = "b) never"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "\n" ++
                        "a) foldl  (++) \"\" (map show [1..5])\n" ++
                        "b) foldl ((++) . show) \"\" [1..5]\n" ++
                        "c) foldl ((++) . show) '' [1..5]\n" ++
                        "d) foldl ((++) . show) [] [1..5]\n"
                 , question = question4
                 , result_lhs = "a) foldl  (++) \"\" (map show [1..5])"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "\n" ++
                        "a) foldr const 0 \"tacos\"\n" ++
                        "b) foldr const \" \" \"tacos\"\n" ++
                        "c) foldr const ' ' \"tacos\"\n" ++
                        "d) foldr const \"\" \"tacos\"\n"
                 , question = question4
                 , result_lhs = "c) foldr const ' ' \"tacos\""
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "\n" ++
                        "a) foldl (flip const) 0 \"burritos\"\n" ++
                        "b) foldl (flip const) ' ' \"burritos\"\n" ++
                        "c) foldl (flip const) \"\" \"burritos\"\n" ++
                        "d) foldl (flip const) \" \" \"burritos\"\n"
                 , question = question4
                 , result_lhs = "b) foldl (flip const) ' ' \"burritos\""
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "\n" ++
                        "a) foldl (flip const) 0 [1..5]\n" ++
                        "b) foldl (flip const) 'z' [1..5]\n" ++
                        "c) foldl (flip const) \"z\" [1..5]\n" ++
                        "d) foldl (flip const) z [1..5]\n"
                 , question = question4
                 , result_lhs = "a) foldl (flip const) 0 [1..5]"
                 , yes_no = "a"
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
