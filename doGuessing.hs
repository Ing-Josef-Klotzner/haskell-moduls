doGuess :: (Ord a, Read a) => a -> IO ()
doGuess num = doGuessing num 1

doGuessing :: (Num a1, Ord a, Read a, Show a1) => a -> a1 -> IO ()
doGuessing num ct = do
    putStrLn "Enter your guess:"
    guess <- getLine
    case compare (read guess) num of
        LT -> do putStrLn "Too low!"
                 doGuessing num (ct + 1)
        GT -> do putStrLn "Too high!"
                 doGuessing num (ct + 1)
        EQ -> putStrLn $ "You Win! with " ++ show ct ++ " attempts"
