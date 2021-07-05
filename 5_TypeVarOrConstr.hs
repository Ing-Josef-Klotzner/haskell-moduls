-- 5_TypeVarOrConstr.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
module TypeVarOrConstr where



question1 = "polymorphic (p), constrained (c) or concrete (o)type?"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "f :: zed -> Zed -> Blah\n\n" ++
                            "a) zed (c), Zed (p), Blah (p)'\n" ++
                            "b) zed (p), Zed (o), Blah (o)\n" ++
                            "c) zed (o), Zed (c), Blah (c)"
                 , question = question1
                 , result_lhs = "b) zed (p), Zed (o), Blah (o)"
                 , yes_no = 'b'
                 }
    , Challenge { lhs = "f :: Enum b => a -> b -> C\n\n" ++
                        "a) a (o), b (p), C (c)\n" ++
                        "b) a (c), b (o), C (p)\n" ++
                        "c) a (p), b (c), C (o)"
                 , question = question1
                 , result_lhs = "c) a (p), b (c), C (o)\n"
                 , yes_no = 'c'
                 }
    , Challenge { lhs = "f :: f -> g -> C\n\n" ++
                        "a) f (p), g (p), C (o)\n" ++
                        "b) f (o), g (o), C (c)\n" ++
                        "c) f (c), g (c), C (o)"
                 , question = question1
                 , result_lhs = "a) f (p), g (p), C (o)\n"
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
