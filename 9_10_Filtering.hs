-- 9_10_Exercises.hs
module Filtering_9_10 where

filterMultiplesOf3 = filter (\x -> (rem x 3) == 0) [1..30]
filterArticles xs = filter (\x -> not $ elem x ["the","a","an"]) $ words xs

question1 = "\nWrite a filter function that would give us all the multiples of 3 out of a list from 1-30"
question2 = "Recalling what we learned about function composition,\n" ++
            "how could we compose the above function (filterMultiplesOf3) with the length\n" ++
            "function to tell us *how many* multiples of 3 there are between 1 and 30"
question3 = "Next we’re going to work on removing all articles (’the’, ’a’,\n" ++
            "and ’an’) from sentences. You want to get to something that works like this:\n" ++
            "Prelude> filterArticles \"the brown dog was a goof\"\n" ++
            "[\"brown\",\"dog\",\"was\",\"goof\"]"
question4 = ""

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) filterMultiplesOf3 = filter (\\x -> (mod x 3) /= 0) [1..30]\n" ++
                        "b) filterMultiplesOf3 = filter (\\x -> (rem x 3) == 0) [1..30]\n" ++
                        "c) filterMultiplesOf3 = filter (\\x -> (rem x 3) == 3) [1..30]\n" ++
                        "d) filterMultiplesOf3 = filtre (\\x -> (rem x 3) == 0) [1..30]"
                 , question = question1
                 , result_lhs = "b) filterMultiplesOf3 = filter (\\x -> (rem x 3) == 0) [1..30]\n" ++
                                "[3,6,9,12,15,18,21,24,27,30]\n"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "a) len filterMultiplesOf3\n" ++
                        "b) length filter MultiplesOf3\n" ++
                        "c) length filterMultiplesOf3\n" ++
                        "d) length filter (\\x -> (mod x 3) == 0) [1..30]"
                 , question = question2
                 , result_lhs = "c) length filterMultiplesOf3\n" ++
                                "10\n"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "map (^2) [1..10]\n" ++
                        "a) filterArticles = filter (\\x -> not $ elem x [\"the\",\"a\",\"an\"]) $ words xs\n" ++
                        "b) filterArticles = filter (\\x -> not elem x [\"the\",\"a\",\"an\"]) $ words xs\n" ++
                        "c) filterArticles = filter (\\x -> elem x [\"the\",\"a\",\"an\"]) $ words xs\n" ++
                        "d) filterArticles = filter (\\x -> x elem [\"the\",\"a\",\"an\"]) $ words xs\n"
                 , question = question3
                 , result_lhs = "a) filterArticles = filter (\\x -> not $ elem x [\"the\",\"a\",\"an\"]) $ words xs"
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
