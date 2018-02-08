-- 6_14_MultipleChoice.hs
module MultipleChoice where

question1 = "The Eq class"
question2 = "The typeclass Ord"
question3 = "Suppose the typeclass Ord has an operator >. What is the type of >?"
question4 = "In x = divMod 16 12"
question5 = "The typeclass Integral includes"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "a) includes all types in Haskell\n" ++
                        "b) is the same as the Ord class\n" ++
                        "c) makes equality tests possible\n" ++
                        "d) only includes numeric types"
                 , question = question1
                 , result_lhs = "c) makes equality tests possible"
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "a) allows any two values to be compared\n" ++
                        "b) is a subclass of Eq\n" ++
                        "c) is a superclass of Eq\n" ++
                        "d) has no instance for Bool"
                 , question = question2
                 , result_lhs = "b) is a subclass of Eq"
                 , yes_no = 'b'
                 }
     , Challenge { lhs = "a) Ord a => a -> a -> Bool\n" ++
                        "b) Ord a => Int -> Bool\n" ++
                        "c) Ord a => a -> Char\n" ++
                        "d) Ord a => Char -> [Char]"
                 , question = question3
                 , result_lhs = "a) Ord a => a -> a -> Bool"
                 , yes_no = 'a'
                 }
     , Challenge { lhs = "a) the type of x is Integer\n" ++
                        "b) the value of x is undecidable\n" ++
                        "c) the type of x is a tuple\n" ++
                        "d) x is equal to 12 / 16"
                 , question = question4
                 , result_lhs = "c) takes a tuple argument and returns the first value"
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "a) Int and Integer numbers\n" ++
                        "b) integral, real, and fractional numbers\n" ++
                        "c) Schrodinger’s cat\n" ++
                        "d) only positive numbers"
                 , question = question5
                 , result_lhs = "a) Int and Integer numbers"
                 , yes_no = 'a'
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
