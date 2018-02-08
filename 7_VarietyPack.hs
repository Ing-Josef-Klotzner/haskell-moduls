-- 7_VarietyPack.hs
module VarietyPack where

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

question1 = "\nGiven the following declarations\n" ++
            "k (x, y) = x\n" ++
            "k1 = k ((4-1), 10)\n" ++
            "k2 = k (\"three\", (1 + 2))\n" ++
            "k3 = k (3, True)\n\n" ++
            "What is the type of k?"
question2 = "\nGiven the following declarations\n" ++
            "k (x, y) = x\n" ++
            "k1 = k ((4-1), 10)\n" ++
            "k2 = k (\"three\", (1 + 2))\n" ++
            "k3 = k (3, True)\n\n" ++
            "What is the type of k2? Is it the same type as k1 or k3?"
question3 = "Of k1, k2, k3, which will return the number 3 as the result?"
question4 = "Fill in the definition of the following function:\n" ++
            "f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))\n" ++
            "f = undefined"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) k :: (t, t1) -> t1\n" ++
                        "b) k :: (a, b) -> a\n" ++
                        "c) k :: (a, b) -> b\n" ++
                        "d) k :: (a, b) -> c"
                 , question = question1
                 , result_lhs = "b) k :: (a, b) -> a"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) Int\n" ++
                        "b) String\n" ++
                        "c) Integer\n" ++
                        "d) Fractional"
                 , question = question2
                 , result_lhs = "b) String"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'bc' (the right combination) and enter\n" ++
                        "a) k1\n" ++
                        "b) k2\n" ++
                        "c) k3"
                 , question = question3
                 , result_lhs = "a) and c) have both type Integer"
                 , yes_no = "ac"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) f (a, b, c) (d, e, f) = ((a, d) (c, f))\n" ++
                        "b) f (a, b, c) (d, e, f) = ((a, d); (c, f))\n" ++
                        "c) f (a, b, c) (d, e, f) = ((a, d), (c, f))\n" ++
                        "d) f (a, b, c) (d, e, f) = ((a, c), (d, f))"
                 , question = question4
                 , result_lhs = "c) f (a, b, c) (d, e, f) = ((a, d), (c, f))"
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
