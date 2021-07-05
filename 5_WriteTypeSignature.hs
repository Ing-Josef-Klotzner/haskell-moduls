-- 5_WriteTypeSignature.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
module WriteTypeSignature where

functionH :: [a] -> a
functionH (x:_) = x

functionC x y = if (x > y) then True else False
functionC :: Ord a => a -> a -> Bool

functionS :: (a, b) -> b
functionS (x, y) = y

question = "\nWhat is Typesignature for follwing function?\n" ++
        "Think about and if you think to know press space button to see if you are right"
a2 = "it is\n"

data Challenge = Challenge { function :: String
                           , func_sig :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { function = "functionH (x:_) = x"
                 , func_sig = "functionH :: [a] -> a"
                 }
     , Challenge { function = "functionC x y = if (x > y) then True else False"
                 , func_sig = "functionC :: Ord a => a -> a -> Bool"
                 }
     , Challenge { function = "functionS (x, y) = y"
                 , func_sig = "functionS :: (a, b) -> b"
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ question
                         , ""
                         , function c
                         , ""
                         ]

showResult :: Challenge -> String
showResult c = unlines [ a2
                         , func_sig c
                         ]

execChallenge :: Challenge -> IO ()
execChallenge c = do
  putStr $ askChallenge c
  answer <- getChar
  putStrLn ""
  putStrLn $ showResult c

main = do
    rs <- mapM execChallenge xs
    putStrLn $ "Congratulations, you are done"
