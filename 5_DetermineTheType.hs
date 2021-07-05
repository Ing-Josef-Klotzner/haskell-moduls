-- 5_Exercises.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where
-- simple example
example = 1
c1 :: Num a => a
c1 = (* 9) 6
c2 :: Num a => (a, [Char])
c2 = head [(0,"doge"),(1,"kitteh")]
c3 :: (Integer, [Char])
c3 = head [(0 :: Integer ,"doge"),(1,"kitteh")]
c4 :: Bool
c4 = if False then True else False
c5 :: Int
c5 = length [1, 2, 3, 4, 5]
c6 :: Bool
c6 = (length [1, 2, 3, 4]) > (length "TACOCAT")
x :: Num a => a
x = 5
y :: Num a => a
y = x + 5
z :: Num a => a
z = y * 10
r :: Num a => a
r = 5
s :: Num a => a
s = r + 5
t :: Num a => a -> a
t s = s * 10
u :: Num a => a
u = 5
v :: Num a => a
v = u + 5
w :: Fractional a => a
w = 4 / v
o :: [Char]
o = "Julie"
p :: [Char]
p = " <3 "
q :: [Char]
q = "Haskell"
f :: [Char]
f = o ++ p ++ q

question1 = "All function applications return a value. Determine the\n" ++
            "value returned by these function applications and the type\n" ++
            "of that value"
question2 = ""
question3 = ""
question4 = ""
data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: Char
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "the challenge was done in writing the source code" ++
                        "\nhave a look there - it won't bite :)" ++
                        "\nif you want to see the results press y"
                 , question = question1
                 , result_lhs = "c1 = (* 9) 6 = " ++ show c1 ++
                                "\nc2 = head [(0,\"doge\"),(1,\"kitteh\")] = " ++ show c2 ++
                                "\nc3 = head [(0 :: Integer ,\"doge\"),(1,\"kitteh\")] = " ++ show c3 ++
                                "\nc4 = if False then True else False = " ++ show c4 ++
                                "\nc5 = length [1, 2, 3, 4, 5] = " ++ show c5 ++
                                "\nc6 = (length [1, 2, 3, 4]) > (length \"TACOCAT\") = " ++ show c6
                 , yes_no = 'y'
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
