-- 5_TypeArguments.hs
module TypeArguments where

question1 = "If the type of f is a -> a -> a -> a, and the type of x is Char then the type of f x is"
question2 = "If the type of g is a -> b -> c -> b, then the type of g 0 'c' \"woot\" is"
question3 = "If the type of h is (Num a, Num b) => a -> b -> b, then the type of   h 1.0 2 is:"
question4 = "If the type of h is (Num a, Num b) => a -> b -> b, then the type of   h 1 (5.5 :: Double) is"
question5 = "If the type of jackal is (Ord a, Eq b) => a -> b -> a, then" ++
            "\nthe type of jackal \"keyboard\" \"has the word jackal in it\""
question6 = "If the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of jackal \"keyboard\""
question7 = "If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of   kessel 1 2 is"
question8 = "If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of" ++
            "\nkessel 1 (2 :: Integer)"
question9 = "If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of" ++
            "\nkessel (1 :: Integer) 2 is"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "a) Char -> Char -> Char" ++
                        "\nb) x -> x -> x -> x" ++
                        "\nc) a -> a -> a" ++
                        "\nd) a -> a -> a -> Char"
                 , question = question1
                 , result_lhs = "a) Char -> Char -> Char"
                 , yes_no = 'a'  -- verified with ghci
                 }
     , Challenge { lhs = "a) String" ++
                        "\nb) Char -> String" ++
                        "\nc) Int" ++
                        "\nd) Char"
                 , question = question2
                 , result_lhs = "d) Char"
                 , yes_no = 'd'  -- verified with ghci
                 }
     , Challenge { lhs = "a) Double" ++
                        "\nb) Integer" ++
                        "\nc) Integral b => b" ++
                        "\nd) Num b => b"
                 , question = question3
                 , result_lhs = "d) Num b => b"
                 , yes_no = 'd'
                 }
     , Challenge { lhs = "a) Integer" ++
                        "\nb) Fractional b => b" ++
                        "\nc) Double" ++
                        "\nd) Num b => b"
                 , question = question4
                 , result_lhs = "c) Double"
                 , yes_no = 'c'
                 }
     , Challenge { lhs = "a) [Char]" ++
                        "\nb) Eq b => b" ++
                        "\nc) b -> [Char]" ++
                        "\nd) b" ++
                        "\ne) Eq b => b -> [Char]"
                 , question = question5
                 , result_lhs = "a) [Char]"
                 , yes_no = 'a'
                 }
     , Challenge { lhs = "a) b" ++
                        "\nb) Eq b => b" ++
                        "\nc) [Char]" ++
                        "\nd) b -> [Char]" ++
                        "\ne) Eq b => b -> [Char]"
                 , question = question6
                 , result_lhs = "e) Eq b => b -> [Char]"
                 , yes_no = 'e'
                 }
     , Challenge { lhs = "a) Integer" ++
                        "\nb) Int" ++
                        "\nc) a" ++
                        "\nd) (Num a, Ord a) => a" ++
                        "\ne) Ord a => a" ++
                        "\nf) Num a => a"
                 , question = question7
                 , result_lhs = "d) (Num a, Ord a) => a"
                 , yes_no = 'd'
                 }
     , Challenge { lhs = "a) (Num a, Ord a) => a" ++
                        "\nb) Int" ++
                        "\nc) a" ++
                        "\nd) Num a => a" ++
                        "\ne) Ord a => a" ++
                        "\nf) Integer"
                 , question = question8
                 , result_lhs = "a) (Num a, Ord a) => a"
                 , yes_no = 'a'
                 }
     , Challenge { lhs = "a) Num a => a" ++
                        "\nb) Ord a => a" ++
                        "\nc) Integer" ++
                        "\nd) (Num a, Ord a) => a" ++
                        "\ne) a"
                 , question = question9
                 , result_lhs = "c) Integer"
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
