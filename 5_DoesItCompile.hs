-- 5_DoesItCompile.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
module DoesItCompile where

bigNum x = (^) 5 x
wahoo = bigNum 10

x = print
y = print "woohoo!"
z = x "hello world"

a = (+)
b = 5
c = b + 10
d = a b c + 200

a' = 12
b' = 10000 * a'

question1 = "Will it compile? (y/n)"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "bigNum = (^) 5 $ 10\n" ++
                        "wahoo = bigNum $ 10"
                 , question = question1
                 , result_lhs = "Couldn't match expected type `a0 -> t0' with actual type `Integer'\n" ++
                            "The first argument of ($) takes one argument,\n" ++
                            "but its type `Integer' has none\n" ++
                            "In the expression: bigNum $ 10\n" ++
                            "In an equation for `wahoo': wahoo = bigNum $ 10\n" ++
                            "\npossible fix:\n" ++
                            "bigNum x = (^) 5 x\n" ++
                            "wahoo = bigNum 10\n\n" ++
                            "wahoo = " ++ show wahoo
                 , yes_no = 'n'
                 }
    , Challenge { lhs = "x = print\n" ++
                        "y = print \"woohoo!\"\n" ++
                        "z = x \"hello world\""
                 , question = question1
                 , result_lhs = "y\n" ++
                            "\"wohoo!\"\n" ++
                            "z\n" ++
                            "\"hello world\"\n"
                 , yes_no = 'y'
                 }
    , Challenge { lhs = "a = (+)\n" ++
                        "b = 5\n" ++
                        "c = b 10\n" ++
                        "d = c 200"
                 , question = question1
                 , result_lhs = "Couldn't match expected type `a0 -> t0' with actual type `Integer'\n" ++
                                "The function `b' is applied to one argument,\n" ++
                                "but its type `Integer' has none\n" ++
                                "In the expression: b 10\n" ++
                                "In an equation for `c': c = b 10\n\n" ++
                                "\npossible fix:\n" ++
                                "a = (+)\n" ++
                                "b = 5\n" ++
                                "c = b + 10\n" ++
                                "d = a b c + 200\n\n" ++
                                "d = " ++ show d
                 , yes_no = 'n'
                 }
    , Challenge { lhs = "a = 12 + b\n" ++
                        "b = 10000 * c"
                 , question = question1
                 , result_lhs = "Not in scope: `b'\n" ++
                                "Not in scope: `c'\n\n" ++
                                "\npossible fix:\n" ++
                                "a = 12\n" ++
                                "b = 10000 * a\n\n" ++
                                "b = " ++ show b'
                 , yes_no = 'n'
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
