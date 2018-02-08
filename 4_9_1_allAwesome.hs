-- 4_9_1_allAwesome
module AllAwesome where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
question1 = "a) length ;; [a] -> Int" ++
            "\nb) Length :: [a] -> Int" ++
            "\nc) length :: [a] -> Int" ++
            "\nd) length :: [a] -> Num"
question2 = "What is the result of the following expression?"
question3 = "Does this work? (y/n)"
question4 = "a) 6 / length [1, 2, 3]" ++
            "\nb) 6 `div` length [1,2,3]" ++
            "\nc) 6 quot length [1,2,3]" ++
            "\nd) `div` 6 length [1,2,3]" ++
            "\ne) other solution"
question5 = "What is the type of the expression 2 + 3 == 5?" ++
            "\n\na) Wool" ++
            "\nb) Fool" ++
            "\nc) Bool" ++
            "\nd) Fint" ++
            "\ne) Int"
question6 = "What would we expect as a result for 2 + 3 == 5?" ++
            "\n\na) False" ++
            "\nb) Yes" ++
            "\nc) Real" ++
            "\nd) True" ++
            "\ne) Love"
question7 = "What is the type and expected result value of the following: " ++
            "\nlet x = 5" ++
            "\nx + 3 == 5" ++
            "\n\na) Int 8" ++
            "\nb) Integer 5" ++
            "\nc) Bool False" ++
            "\nd) Bool True" ++
            "\ne) Cool True"
question8 = "Will this code work? Why or why not?" ++
            "\nIf it will work, what value would it reduce to?"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "\ntype signature of function length is? (a-d):"
                 , question = question1
                 , result_lhs = "length :: [a] -> Int"
                 , yes_no = 'c'  -- verified with ghci
                 }
     , Challenge { lhs = "length [1, 2, 3, 4, 5]"
                 , question = question2
                 , result_lhs = "length: " ++ show (length [1, 2, 3, 4, 5])
                 , yes_no = '5'  -- verified with ghci
                 }
     , Challenge { lhs = "length [(1, 2), (2, 3), (3, 4)]"
                 , question = question2
                 , result_lhs = "length: " ++ show (length [(1, 2), (2, 3), (3, 4)])
                 , yes_no = '3'
                 }
     , Challenge { lhs = "allAwesome = [awesome, also]" ++
                        "\nlength allAwesome"
                 , question = question2
                 , result_lhs = "length: " ++ show (length allAwesome)
                 , yes_no = '2'
                 }
     , Challenge { lhs = "awesome = [\"Papuchon\", \"curry\", \":)\"]" ++
                        "\nalso = [\"Quake\", \"The Simons\"]" ++
                        "\nallAwesome = [awesome, also]" ++
                        "\nlength (concat allAwesome)"
                 , question = question2
                 , result_lhs = "length: " ++ show (length (concat allAwesome))
                 , yes_no = '5'
                 }
     , Challenge { lhs = "6 / length [1, 2, 3]"
                 , question = question3
                 , result_lhs = "No instance for (Fractional Int) arising from a use of `/'" ++
                                "Possible fix: add an instance declaration for (Fractional Int)" ++
                                "In the expression: 6 / length [1, 2, 3]" ++
                                "In an equation for `it': it = 6 / length [1, 2, 3]"

                 , yes_no = 'n'
                 }
     , Challenge { lhs = "Which one will work? (a-e)"
                 , question = question4
                 , result_lhs = "answer: " ++ show (6 `div` length [1,2,3])
                 , yes_no = 'b'
                 }
     , Challenge { lhs = "Select type (a-e)"
                 , question = question5
                 , result_lhs = "answer: Bool"
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "Enter result (a-e)"
                 , question = question6
                 , result_lhs = "answer: " ++ show (2 + 3 == 5)
                 , yes_no = 'd'
                 }
     , Challenge { lhs = "Select type and result (a-e)"
                 , question = question7
                 , result_lhs = "answer: Bool " ++ show (5 + 3 == 5)
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "length allAwesome == 2" ++
                        "\n\na) Will not work" ++
                        "\nb) Result is True" ++
                        "\nc) Result is 2" ++
                        "\nd) Result is all Awesome" ++
                        "\ne) Result is equal"
                 , question = question8
                 , result_lhs = "answer: " ++ show (length allAwesome == 2)
                 , yes_no = 'b'
                 }
     , Challenge { lhs = "length [1, 'a', 3, 'b']" ++
                        "\n\na) Will not work" ++
                        "\nb) Result is True" ++
                        "\nc) Result is 2" ++
                        "\nd) Result is 4" ++
                        "\ne) Result is equal"
                 , question = question8
                 , result_lhs = "No instance for (Num Char) arising from the literal `1'" ++
                                "\nPossible fix: add an instance declaration for (Num Char)" ++
                                "\nIn the expression: 1" ++
                                "\nIn the first argument of `length', namely `[1, 'a', 3, 'b']'" ++
                                "\nIn the expression: length [1, 'a', 3, 'b']"

                 , yes_no = 'a'
                 }
     , Challenge { lhs = "awesome = [\"Papuchon\", \"curry\", \":)\"]" ++
                        "\nallAwesome = [awesome, also]" ++
                        "\nlength allAwesome + length awesome" ++
                        "\n\na) Will not work" ++
                        "\nb) Result is awesome" ++
                        "\nc) Result is 5" ++
                        "\nd) Result is 1" ++
                        "\ne) Result is 2"
                 , question = question8
                 , result_lhs = "answer: " ++ show (length allAwesome + length awesome)
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "(8 == 8) && ('b' < 'a')" ++
                        "\n\na) Will not work" ++
                        "\nb) Result is unknown" ++
                        "\nc) Result is unpure" ++
                        "\nd) Result is True" ++
                        "\ne) Result is False"
                 , question = question8
                 , result_lhs = "answer: " ++ show ((8 == 8) && ('b' < 'a'))
                 , yes_no = 'e'
                 }
     , Challenge { lhs = "(8 == 8) && 9" ++
                        "\n\na) Will not work" ++
                        "\nb) Result is unknown" ++
                        "\nc) Result is unpure" ++
                        "\nd) Result is True" ++
                        "\ne) Result is False"
                 , question = question8
                 , result_lhs = "No instance for (Num Bool) arising from the literal `9'" ++
                                "\nPossible fix: add an instance declaration for (Num Bool)" ++
                                "\nIn the second argument of `(&&)', namely `9'" ++
                                "\nIn the expression: (8 == 8) && 9" ++
                                "\nIn an equation for `it': it = (8 == 8) && 9"
                 , yes_no = 'a'
                 }
     ]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then
            -x
        else
            x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y),(fst x, fst y))

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
--    putStrLn $ "\ntype signature of length:" ++
--            "\nlength :: [a] -> Int   (written by heart - i swear! :=) )"
    rs <- mapM execChallenge xs
    let points = length . filter id $ rs
    let possiblePoints = length xs
    putStrLn $ "Congratulations, scored " ++ show points ++ "/" ++ show possiblePoints ++ " (" ++ 
        show (fromIntegral 100 * fromIntegral points/ fromIntegral possiblePoints) ++ "%)" ++ "!"
    putStrLn $ "\nFinally to relax there is a function checking palindromes - give me string to check "
    y <- getLine
    if isPalindrome y then
        putStrLn $ show y ++ "is a palindrome, reading the same from left to right and right to left"
    else
        putStrLn $ show y ++ "is no palindrome - there is different words from left to right or vice versa"
    putStrLn $ "\nAnd here is a function returning absolute value - give me a number"
    y <- readLn
    putStrLn $ show y ++ " absolute value: " ++ show (myAbs y)
    putStrLn $ "To very ending i give you a function for use: enjoy! " ++
                "\nf :: (a, b) -> (c, d) -> ((b, d), (a, c))" ++
                "\nf x y = ((snd x, snd y),(fst x, fst y))"
