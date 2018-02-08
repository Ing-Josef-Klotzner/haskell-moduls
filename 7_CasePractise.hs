-- 7_CasePractise.hs
module CasePractise where

--Rewrite if-then-else expressions into case expressions.
--1. The following should return x when x is greater than y, else return y.
functionC x y = if (x > y) then x else y

functionC' x y = case (x > y) of
                True -> x
                False -> y

--2. The following will add 2 to even numbers and otherwise
--simply return the input value.
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n = case even n of 
                True -> n + 2 
                False -> n

--3. The following compares a value, x, to zero and returns an
--indicator for whether x is a postive number or negative
--number. But what if x is 0? You may need to play with
--the compare function a bit to find what to do.
nums x =
    case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0   -- added to solve



question1 = "\nFirst, rewrite if-then-else expressions into case expressions.\n" ++
            "1. The following should return x when x is greater than y, else return y.\n\n" ++
            "functionC x y = if (x > y) then x else y"
question2 = "\nRewrite if-then-else expressions into case expressions\n" ++
            "2. The following will add 2 to even numbers and otherwise\n" ++
            "simply return the input value.\n\n" ++
            "ifEvenAdd2 n = if even n then (n+2) else n\n"
question3 = "\n3. The following compares a value, x, to zero and returns an\n" ++
            "indicator for whether x is a postive number or negative\n" ++
            "number. But what if x is 0? You may need to play with\n" ++
            "the compare function a bit to find what to do" ++
            "\nnums x =\n" ++
            "    case compare x 0 of\n" ++
            "    LT -> -1\n" ++
            "    GT -> 1"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a)  functionC' x y = case (x > y) where\n" ++
                        "                     True -> x\n" ++
                        "                     False -> y\n" ++
                        "b)  functionC' x y = case (x < y) of\n" ++
                        "                     True -> x\n" ++
                        "                     False -> y\n" ++
                        "c)  functionC' x y = case (x > y) of\n" ++
                        "                     True -> x\n" ++
                        "                     False -> y\n" ++
                        "d)  functionC' x y = käse (x > y) of\n" ++
                        "                     True -> x\n" ++
                        "                     False -> y"
                 , question = question1
                 , result_lhs = "c) is correct"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) ifEvenAdd2' n = case even n' of\n" ++
                        "                   True -> n + 2\n" ++
                        "                   False -> n\n" ++
                        "b) ifEvenAdd2' n = case ewen n of\n" ++
                        "                   True -> n + 2\n" ++
                        "                   False -> n\n" ++
                        "c) ifEvenAdd2' n' = case even n of\n" ++
                        "                   True -> n + 2\n" ++
                        "                   False -> n\n" ++
                        "d) ifEvenAdd2' n = case even n of\n" ++
                        "                   True -> n + 2\n" ++
                        "                   False -> n\n"
                 , question = question2
                 , result_lhs = "d) is correct"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) nums x = case compair x 0 of\n" ++
                        "      LT -> -1\n" ++
                        "      GT -> 1\n" ++
                        "      EQ -> 0\n" ++
                        "b) nums x = case compare x 0 of\n" ++
                        "      LT -> -10\n" ++
                        "      GT -> 1\n" ++
                        "      EQ -> 0\n" ++
                        "c) nums x = case compare x 0 of\n" ++
                        "      LT -> -1\n" ++
                        "      GT -> 1\n" ++
                        "      EQ -> 0\n" ++
                        "d) nums x = case compare x 0 of\n" ++
                        "      LT -> -1\n" ++
                        "      GT -> 1\n" ++
                        "      == -> 0"
                 , question = question3
                 , result_lhs = "b) and c) are correct  :P"
                 , yes_no = "bc"
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
