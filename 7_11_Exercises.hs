-- 7_11_Exercises.hs
module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = snd $ divMod (fst $ divMod x 10) 10

tensDigit'' :: Integral a => a -> a
tensDigit'' = snd . flip divMod 10 . fst . flip divMod 10

hunsDigit :: Integral b => b -> b
hunsDigit = snd . flip divMod 10 . fst . flip divMod 100

hunsDigit' :: Integral b => b -> b
hunsDigit' = snd . (`divMod` 10) . fst . (`divMod` 100)

hunsDigit'' :: Integral b => b -> b
hunsDigit'' = (`mod` 10) . (`div` 100)
 
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool3' :: a -> a -> Bool -> a
foldBool3' x y b = case b of
    False -> x
    True  -> y

foldBool3'' :: a -> a -> Bool -> a
foldBool3'' x y b 
    | b == False = x
    | b == True  = y

g :: (a -> b) -> (a, c) -> (b, c)
g aTob (a, c) = (aTob a, c) 

roundTrip :: (Show a, Read a) => a -> a
roundTrip = (read . show)

roundTrip' :: (Num b, Show a, Read b) => a -> b
roundTrip' = (read . show)

question1 = "\nMultiple choice"
question2 = "The following function returns the tens digit of an integral argument.\n" ++
            "tensDigit :: Integral a => a -> a\n" ++
            "tensDigit x = d\n" ++
            "   where xLast = x `div` 10\n" ++
            "             d = xLast `mod` 10"
question3 = "Implement the function of the type a -> a -> Bool -> a\n" ++
            "using a case expression\n" ++
            "foldBool :: a -> a -> Bool -> a\n" ++
            "foldBool3 x _ False = x\n" ++
            "foldBool3 _ y True = y\n"
question4 = "Implement the function of the type a -> a -> Bool -> a\n" ++
            "using a guard.\n" ++
            "foldBool :: a -> a -> Bool -> a\n" ++
            "foldBool3 x _ False = x\n" ++
            "foldBool3 _ y True = y\n"
question5 = "Fill in the definition. Note that the first argument to our\n" ++
            "function is also a function which can be applied to values.\n" ++
            "Your second argument is a tuple, which can be used for\n" ++
            "pattern matching:\n" ++
            "g :: (a -> b) -> (a, c) -> (b, c)\n" ++
            "g = undefined"
question6 = "For this next exercise, you’ll experiment with writing\n" ++
            "pointfree versions of existing code. This involves some new information,\n" ++
            "so read the following explanation carefully.\n" ++
            "Typeclasses are dispatched by type. Read is a typeclass like\n" ++
            "Show, but it is the dual or “opposite” of Show. In general, the\n" ++
            "Read typeclass isn’t something you should plan to use a\n" ++
            "lot, but this exercise is structured to teach you something\n" ++
            "about the interaction between typeclasses and types.\n" ++
            "The function read in the Read typeclass has the type:\n" ++
            "read :: Read a => String -> a\n" ++
            "Notice a pattern?\n" ++
            "read :: Read a => String -> a\n" ++
            "show :: Show a => a -> String\n" ++
            "Following transformed to pointfree version\n" ++
            "roundTrip :: (Show a, Read a) => a -> a\n" ++
            "roundTrip a = read (show a)"
question7 = "roundTrip :: (Show a, Read a) => a -> a\n" ++
            "roundTrip = (read . show) \n" ++
            "Your task now is to change the type of roundTrip to\n" ++
            "roundTrip :: (Show a, Read b) => a -> b"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "A polymorphic function\n" ++
                        "a) changes things into sheep when invoked\n" ++
                        "b) has multiple arguments\n" ++
                        "c) has a concrete type\n" ++
                        "d) may resolve to values of different types, depending on inputs"
                 , question = question1
                 , result_lhs = "d) may resolve to values of different types, depending on inputs"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "Two functions named f and g have types Char -> String\n" ++
                        "and String -> [String] respectively. The composed func-\n" ++
                        "tion g . f has the type\n" ++
                        "a) Char -> String\n" ++
                        "b) Char -> [String]\n" ++
                        "c) [[String]]\n" ++
                        "d) Char -> String -> [String]"
                 , question = question1
                 , result_lhs = "b) Char -> [String]"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "A function f has the type Ord a => a -> a -> Bool and we\n" ++
                        "apply it to one numeric value. What is the type now?\n"++
                        "a) Ord a => a -> Bool\n" ++
                        "b) Num -> Num -> Bool\n" ++
                        "c) Ord a => a -> a -> Integer\n" ++
                        "d) (Ord a, Num a) => a -> Bool"
                 , question = question1
                 , result_lhs = "d) (Ord a, Num a) => a -> Bool"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "A function with the type (a -> b) -> c\n" ++
                        "a) requires values of three different types\n" ++
                        "b) is a higher-order function\n" ++
                        "c) must take a tuple as its first argument\n" ++
                        "d) has its parameters in alphabetical order"
                 , question = question1
                 , result_lhs = "b) is a higher-order function"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "Given the following definition of f, what is the type of f True?\n" ++
                        "f :: a -> a\n" ++
                        "f x = x\n" ++
                        "a) f True :: Bool\n" ++
                        "b) f True :: String\n" ++
                        "c) f True :: Bool -> Bool\n" ++
                        "d) f True :: a"
                 , question = question1
                 , result_lhs = "a) f True :: Bool"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Rewriting using divMod\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) tensDigit x = snd . fst . x `divMod` 10\n" ++
                        "b) tensDigit x = snd $ divMod $ fst x `divMod` 10\n" ++
                        "c) tensDigit x = fst § x `divMod` 10\n" ++
                        "d) tensDigit x = snd $ divMod (fst $ x `divMod` 10) 10"
                 , question = question2
                 , result_lhs = "d) tensDigit x = snd $ divMod (fst $ x `divMod` 10) 10"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Next, let’s change it so that we’re getting the hundreds digit instead.\n" ++
                        "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) hunsDigit x = snd . fst . x `divMod` 100\n" ++
                        "b) hunsDigit x = snd $ divMod (fst $ x `divMod` 100) 10\n" ++
                        "c) hunsDigit x = fst $ (x `mod` 10) `divMod` 10\n" ++
                        "d) hunsDigit x = snd $ divMod $ fst x `divMod` 100\n"
                 , question = question2
                 , result_lhs = "b) hunsDigit x = snd $ divMod (fst $ x `divMod` 100) 10"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) foldBool3 x y b = case b of\n" ++
                        "       False -> x\n" ++
                        "       True  -> y\n" ++
                        "b) foldBool3 b = case x y b of\n" ++
                        "       False -> x\n" ++
                        "       True  -> y\n" ++
                        "c) foldBool3 x y b = case b of\n" ++
                        "       Wrong -> x\n" ++
                        "       Right -> y\n" ++
                        "d) foldbool3 x y b = case b of\n" ++
                        "       False -> x\n" ++
                        "       True  -> y\n"
                 , question = question3
                 , result_lhs = "a) foldBool3 x y b = case b of\n" ++
                        "       False -> x\n" ++
                        "       True  -> y\n"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) foldBool3 x y b\n" ++
                        "       | b == False = y\n" ++
                        "       | b == True  = x\n" ++
                        "b) foldBool3 x y b\n" ++
                        "       | b == Fals  = x\n" ++
                        "       | b == True  = y\n" ++
                        "c) foldBool3 x y b\n" ++
                        "       | b == False = x\n" ++
                        "       | b == True  = y\n" ++
                        "d) foldbool3 y x b\n" ++
                        "       | b == False = x\n" ++
                        "       | b == True  = y\n"
                 , question = question4
                 , result_lhs = "c) foldBool3 x y b\n" ++
                        "       | b == False = x\n" ++
                        "       | b == True  = y"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) g aTob (a, b) = (aTob a, c)\n" ++
                        "b) g aTob (a, c) = (aTob a, c)\n" ++
                        "c) g aTob (a, c) = aTob b (a, c)\n" ++
                        "d) g aTob (a, c) = (b, c)\n"
                 , question = question5
                 , result_lhs = "b) g aTob (a, c) = (aTob a, c)"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) roundTrip = (read $ show)\n" ++
                        "b) roundTrip = (read | show)\n" ++
                        "c) roundTrip = (read . show)\n" ++
                        "d) roundTrip x = (read . show) x\n"
                 , question = question6
                 , result_lhs = "c) roundTrip = (read . show)"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) roundTrip' :: (Num b, Show a, Read b) => a -> b\n" ++
                        "   roundTrip' = (read . show)\n" ++
                        "b) roundTrip' :: (Num b, Show a, Read b) => a -> b\n" ++
                        "   roundTrip' = (show . read)\n" ++
                        "c) roundTrip' :: (Num b, Show a, Read b) => a <- b\n" ++
                        "   roundTrip' = (read . show)\n" ++
                        "d) roundTrip' :: (Num b, Show a, Read b) => a -> b\n" ++
                        "   roundTrip' = (read (show))"
                 , question = question7
                 , result_lhs = "a) roundTrip' :: (Num b, Show a, Read b) => a -> b\n" ++
                        "   roundTrip' = (read . show)"
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
