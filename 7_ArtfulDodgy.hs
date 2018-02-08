-- 7_ArtfulDodgy.hs
module ArtfulDodgy where

-- Given the following definitions tell us what value results from further applications.

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

question1 = "\nGiven the following definitions tell us what value results from further applications\n\n" ++
            "dodgy :: Num a => a -> a -> a\n" ++
            "dodgy x y = x + y * 10\n\n" ++
            "oneIsOne :: Integer -> Integer\n" ++
            "oneIsOne = dodgy 1\n\n" ++
            "oneIsTwo :: Integer -> Integer\n" ++
            "oneIsTwo = (flip dodgy) 2"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "For example, given the expression dodgy 1 0, what do you\n" ++
                        "think will happen if we evaluate it?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) it returns '1 0'\n" ++
                        "b) it will spit 'no way'\n" ++
                        "c) it returns '1'\n" ++
                        "d) it returns a function returning '1'"
                 , question = question1
                 , result_lhs = "c) it returns '1'"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "dodgy 1 1\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '1 1'\n" ++
                        "b) '2'\n" ++
                        "c) '12'\n" ++
                        "d) '11'"
                 , question = question1
                 , result_lhs = "d) '11'"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "dodgy 2 2\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '22'\n" ++
                        "b) '2'\n" ++
                        "c) '2 2'\n" ++
                        "d) '40'"
                 , question = question1
                 , result_lhs = "a) '22'"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "dodgy 1 2\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '12'\n" ++
                        "b) '21'\n" ++
                        "c) '10'\n" ++
                        "d) '3'"
                 , question = question1
                 , result_lhs = "b) '21'"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "dodgy 2 1\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '21'\n" ++
                        "b) '3'\n" ++
                        "c) '12'\n" ++
                        "d) '11'"
                 , question = question1
                 , result_lhs = "c) '12'"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "oneIsOne 1\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '11'\n" ++
                        "b) '10'\n" ++
                        "c) '01'\n" ++
                        "d) '00'"
                 , question = question1
                 , result_lhs = "a) '11'"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "oneIsOne 2\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '12'\n" ++
                        "b) '21'\n" ++
                        "c) '13'\n" ++
                        "d) '11'"
                 , question = question1
                 , result_lhs = "b) '21'"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "oneIsTwo 1\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '1'\n" ++
                        "b) '21'\n" ++
                        "c) '12'\n" ++
                        "d) '2'"
                 , question = question1
                 , result_lhs = "b) '21'"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "oneIsTwo 2\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '12'\n" ++
                        "b) '2'\n" ++
                        "c) '22'\n" ++
                        "d) '-8'"
                 , question = question1
                 , result_lhs = "c) '22'"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "oneIsOne 3\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '22'\n" ++
                        "b) '32'\n" ++
                        "c) '13'\n" ++
                        "d) '31'"
                 , question = question1
                 , result_lhs = "d) '31'"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Now attempt to determine what the following expressions\n" ++
                        "reduce to. Do it in your head, verify in your REPL after\n" ++
                        "you think you have an answer\n" ++
                        "oneIsTwo 3\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) '11'\n" ++
                        "b) '23'\n" ++
                        "c) '32'\n" ++
                        "d) '31'"
                 , question = question1
                 , result_lhs = "b) '23'"
                 , yes_no = "b"
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
