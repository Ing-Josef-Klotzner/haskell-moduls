-- 7_GrabBag.hs
module GrabBag where

question1 = "Which (two or more) of the following are equivalent?"
question2 = "The type of mTh (above) is Num a => a -> a -> a -> a.\n" ++
            "Which is the type of mTh 3?"
question3 = "Next, we’ll practice writing anonymous lambda syntax.\n" ++
            "For example, one could rewrite:\n" ++
            "addOne x = x + 1\n" ++
            "Into:\n" ++
            "addOne = \\x -> x + 1\n" ++
            "Try to make it so it can still be loaded as a top-level def-\n" ++
            "inition by GHCi. This will make it easier to validate your\n" ++
            "answers.\n" ++
            "Rewrite the f function in the where clause.\n" ++
            "addOneIfOdd n = case odd n of\n" ++
            "   True -> f n\n" ++
            "   False -> n\n" ++
            "   where f n = n + 1"
question4 = "Rewrite the following to use anonymous lambda syntax:\n" ++
            "addFive x y = (if x > y then y else x) + 5"
question5 = "Rewrite the following so that it doesn’t use anonymous lambda syntax:\n" ++
            "mflip f = \\x -> \\y -> f y x"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'ab' or 'acd' and enter\n" ++
                        "a) mTh x y z = x * y * z\n" ++
                        "b) mTh x y = \\z -> x * y * z\n" ++
                        "c) mTh x = \\y -> \\z -> x * y * z\n" ++
                        "d) mTh = \\x -> \\y -> \\z -> x * y * z"
                 , question = question1
                 , result_lhs = "a) b) c) d) are equivalent"
                 , yes_no = "abcd"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) Integer -> Integer -> Integer\n" ++
                        "b) Num a => a -> a -> a -> a\n" ++
                        "c) Num a => a -> a\n" ++
                        "d) Num a => a -> a -> a"
                 , question = question2
                 , result_lhs = "d) Num a => a -> a -> a"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) where f \\n = n + 1\n" ++
                        "b) where f n = \\n + 1\n" ++
                        "c) where f = \\n -> n + 1\n" ++
                        "d) where f = \\n + 1"
                 , question = question3
                 , result_lhs = "c) where f = \\n -> n + 1"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) addFive = \\x > \\y > (if x > y then y else x) + 5\n" ++
                        "b) addFive x = \\x -> \\y -> (if x > y then y else x) + 5\n" ++
                        "c) addFive = \\x -> \\y -> (if x > y then y else x) + 5\n" ++
                        "d) addFive = \\y -> \\x -> (if x > y then y else x) + 5"
                 , question = question4
                 , result_lhs = "c) addFive = \\x -> \\y -> (if x > y then y else x) + 5"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) mflip f x y = y x\n" ++
                        "b) mflip f x y = f y x\n" ++
                        "c) mflip f x y = x -> y -> f y x\n" ++
                        "d) mflip f \\x -> \\y = f y x"
                 , question = question5
                 , result_lhs = "b) mflip f x y = f y x"
                 , yes_no = "b"
                 }
     ]
addFive x y = (if x > y then y else x) + 5
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
