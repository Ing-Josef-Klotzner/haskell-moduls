-- 4_9_3_MatchTypes.hs
module MatchTypes where

question1 = "Which of the following types is the type of show?"
question2 = "Which of the following types is the type of (==)?"
question3 = "Which of the following types is the type of fst?"
question4 = "Which of the following types is the type of (+)?"
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "a, b or c?" ++
                        "\na) show a => a -> String" ++
                        "\nb) Show a -> a -> String" ++
                        "\nc) Show a => a -> String"
                 , question = question1
                 , result_lhs = "Show a => a -> String"
                 , yes_no = 'c'  -- verified with ghci
                 }
     , Challenge { lhs = "a) a -> a -> Bool" ++
                        "\nb) Eq a => a -> a -> Bool" ++
                        "\nc) Eq a -> a -> a -> Bool" ++
                        "\nd) Eq a => A -> Bool"
                 , question = question2
                 , result_lhs = "Eq a => a -> a -> Bool"
                 , yes_no = 'b'  -- verified with ghci
                 }
     , Challenge { lhs = "a) (a, b) -> a" ++
                        "\nb) b -> a" ++
                        "\nc) (a, b) -> b"
                 , question = question3
                 , result_lhs = "(a, b) -> a"
                 , yes_no = 'a'
                 }
     , Challenge { lhs = "a) (+) :: Num a -> a -> a -> Bool" ++
                        "\nb) (+) :: Num a => a -> a -> Bool" ++
                        "\nc) (+) :: num a => a -> a -> a" ++
                        "\nd) (+) :: Num a => a -> a -> a" ++
                        "\ne) (+) :: a -> a -> a"
                 , question = question4
                 , result_lhs = "Num a => a -> a -> a"
                 , yes_no = 'd'
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
