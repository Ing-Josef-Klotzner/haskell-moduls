-- 7_GuardDuty.hs
module GuardDuty where

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
    | xs == reverse xs = True
    | otherwise        = False

numbers :: (Num a1, Num a, Ord a1) => a1 -> a
numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1

question1 = "\nWhat delivers following Guard passing 0.79 as argument?\n\n" ++
            "avgGrade :: (Fractional a, Ord a) => a -> Char\n" ++
            "avgGrade x\n" ++
            "    | y >= 0.9 = 'A'\n" ++
            "    | y >= 0.8 = 'B'\n" ++
            "    | y >= 0.7 = 'C'\n" ++
            "    | y >= 0.59 = 'D'\n" ++
            "    | y < 0.59 = 'F'\n" ++
            "    where y = x / 100"
question2 = "What changed now?\n" ++
            "avgGrade :: (Fractional a, Ord a) => a -> Char\n" ++
            "avgGrade x\n" ++
            "    | y >= 0.9 = 'A'\n" ++
            "    | y >= 0.8 = 'B'\n" ++
            "    | y >= 0.7 = 'C'\n" ++
            "    | y >= 0.59 = 'D'\n" ++
            "    | otherwise = 'F'\n" ++
            "    where y = x / 100"
question3 = "Given the following function\n" ++
            "pal xs\n" ++
            "    | xs == reverse xs = True\n" ++
            "    | otherwise        = False"
question4 = "Given the following function\n" ++
            "numbers x\n" ++
            "   | x < 0  = -1\n" ++
            "   | x == 0 = 0\n" ++
            "   | x > 0  = 1"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) 'C'\n" ++
                        "b) 'B'\n" ++
                        "c) 'D'\n" ++
                        "d) 'A'\n" ++
                        "e) 'F'"
                 , question = question1
                 , result_lhs = "a) 'C'"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) Guard is no Helper function any more\n" ++
                        "b) It does not compile\n" ++
                        "c) It behaves as before\n" ++
                        "d) If God blesses, it will not blow up your Computer!"
                 , question = question2
                 , result_lhs = "c) It behaves as before"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "It returns?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) xs written backwards when it’s True\n" ++
                        "b) True when xs is a palindrome\n" ++
                        "c) False when xs is a palindrome\n" ++
                        "d) False when xs is reversed"
                 , question = question3
                 , result_lhs = "b) True when xs is a palindrome"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "What types of arguments can pal take?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) any a's' in brackets []\n" ++
                        "b) any string\n" ++
                        "c) any list\n" ++
                        "d) any Char"
                 , question = question3
                 , result_lhs = "c) any list"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "What is the type of the function pal?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) pal :: Eq a => a[] -> Bool\n" ++
                        "b) pal :: Eq a => String -> Bool\n" ++
                        "c) pal :: Eq a => [reverse] -> Bool\n" ++
                        "d) pal :: Eq a => [a] -> Bool"
                 , question = question3
                 , result_lhs = "d) pal :: Eq a => [a] -> Bool"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "It returns?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) the value of its argument plus or minus 1\n" ++
                        "b) the negation of its argument\n" ++
                        "c) an indication of whether its argument is a positive or\n" ++
                        "   negative number or zero\n" ++
                        "d) binary machine language"
                 , question = question4
                 , result_lhs = "c) an indication of whether its argument is a positive or\n" ++
                                "   negative number or zero\n"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "What types of arguments can numbers take?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) Int\n" ++
                        "b) Integer\n" ++
                        "c) Num\n" ++
                        "d) Real"
                 , question = question4
                 , result_lhs = "c) Num"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "What is the type of the function numbers?\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) numbers :: (Num a1, Num a, Ord a1) => a1 -> a\n" ++
                        "b) numbers :: Num a => -> a\n" ++
                        "c) numbers :: (Num a1, Num a, Ord a) => a1 -> a\n" ++
                        "d) numbers :: (Num a, Ord a) => a -> a"
                 , question = question4
                 , result_lhs = "a) numbers :: (Num a1, Num a, Ord a1) => a1 -> a"
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
