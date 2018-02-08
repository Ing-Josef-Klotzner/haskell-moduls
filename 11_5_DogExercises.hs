-- 11_5_DogExercises.hs
module DogExercises where

data DogueDeBordeaux doge = DogueDeBordeaux doge
data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

question1 = "\nGiven   data Doggies a = Husky a | Mastiff a deriving (Eq, Show)"
question2 = "Given   data DogueDeBordeaux doge = DogueDeBordeaux doge"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "Is Doggies a type constructor or a data constructor?\n" ++
                        "a) data constructor\n" ++
                        "b) type constructor\n" ++
                        "c) both\n" ++
                        "d) kind of Scooby Doo"
                 , question = question1
                 , result_lhs = "b) type constructor"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the kind of Doggies?\n" ++
                        "a) Doggies :: * -> *\n" ++
                        "b) kind of dog\n" ++
                        "c) Dog :: * -> *\n" ++
                        "d) Scooby Dog :: * -> *"
                 , question = question1
                 , result_lhs = "a) Doggies :: * -> *"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the kind of Doggies String?\n" ++
                        "a) Doggies Tail :: *\n" ++
                        "b) Doggies String :: * -> *\n" ++
                        "c) Doggies String :: *\n" ++
                        "d) Doggies String :: \"\"" 
                 , question = question1
                 , result_lhs = "c) Doggies String :: *"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of Husky 10?\n" ++
                        "a) Husky 10 :: Num a => Dogs a\n" ++
                        "b) Husky 10 :: Num a => Doggies\n" ++
                        "c) Husky 10 :: Num a => a\n" ++
                        "d) Husky 10 :: Num a => Doggies a\n"
                 , question = question1
                 , result_lhs = "d) Husky 10 :: Num a => Doggies a"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of Husky (10 :: Integer)?\n" ++
                        "a) Husky (10 :: Integer) :: Doggies a\n" ++
                        "b) Husky (10 :: Integer) :: Doggies Integer\n" ++
                        "c) Husky (10 :: Integer) :: Doggies Int\n" ++
                        "d) Husky (10 :: Integer) :: Doggies\n"
                 , question = question1
                 , result_lhs = "b) Husky (10 :: Integer) :: Doggies Integer"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of Mastiff \"Scooby Doo\"?\n" ++
                        "a) Mastiff \"Scooby Doo\" :: Doggies' [Char]\n" ++
                        "b) Mastiff \"Scooby Doo\" :: Doggies Char\n" ++
                        "c) Mastiff \"Scooby Doo\" :: Doggies [Char]\n" ++
                        "d) Mastiff \"Scooby Doo\" :: Scooby [Char]\n"
                 , question = question1
                 , result_lhs = "c) Mastiff \"Scooby Doo\" :: Doggies [Char]"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "Is DogueDeBordeaux a type constructor or a data constructor?\n" ++
                        "a) both\n" ++
                        "b) type constructor\n" ++
                        "c) data constructor\n" ++
                        "d) Mastiff \"Scooby Doo\" secret constructor\n"
                 , question = question2
                 , result_lhs = "a) both"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of DogueDeBordeaux?\n" ++
                        "a) DogueDeBordeaux :: doge -> DogueDeBordeaux dog\n" ++
                        "b) DogueDeBordeaux :: doge -> DogueDeBordeaux dogue\n" ++
                        "c) DogueDeBordeaux :: doge -> DogueDeBordeaux doge\n" ++
                        "d) DogueDeBordeaux :: doge -> DogueDeBordeaux doo\n"
                 , question = question2
                 , result_lhs = "c) DogueDeBordeaux :: doge -> DogueDeBordeaux doge"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of DogueDeBordeaux \"doggie!\"\n" ++
                        "a) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux Scooby\n" ++
                        "b) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux [Char]\n" ++
                        "c) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux doge\n" ++
                        "d) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux [String]\n"
                 , question = question2
                 , result_lhs = "b) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux [Char]"
                 , yes_no = "b"
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
