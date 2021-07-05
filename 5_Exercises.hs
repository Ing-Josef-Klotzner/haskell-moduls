-- 5_Exercises.hs
module Exercise where

question1 = "A value of type [a] is"
question2 = "A function of type [[a]] -> [a] could"
question3 = "A function of type [a] -> Int -> a"
question4 = "A function of type (a, b) -> a"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "a) a list of alphabetic characters\n" ++
                        "b) a list of lists\n" ++
                        "c) a list whose elements are all of some type a\n" ++
                        "d) a list whose elements are all of different types\n"
                 , question = question1
                 , result_lhs = "c) a list whose elements are all of some type a"
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "a) take a list of strings as an argument\n" ++
                        "b) transform a character into a string\n" ++
                        "c) transform a string into a list of strings\n" ++
                        "d) take two arguments\n"
                 , question = question2
                 , result_lhs = "a) take a list of strings as an argument"
                 , yes_no = 'a'
                 }
     , Challenge { lhs = "a) takes one argument\n" ++
                        "b) returns one element of type a from a list\n" ++
                        "c) must return an Int value\n" ++
                        "d) is completely fictional\n"
                 , question = question3
                 , result_lhs = "b) returns one element of type a from a list"
                 , yes_no = 'b'
                 }
     , Challenge { lhs = "a) takes a list argument and returns a Char value\n" ++
                        "b) has zero arguments\n" ++
                        "c) takes a tuple argument and returns the first value\n" ++
                        "d) requires that a and b be of different types\n"
                 , question = question4
                 , result_lhs = "c) takes a tuple argument and returns the first value"
                 , yes_no = 'c'
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ question c
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
