-- 5_GivenTypeWriteFunction.hs
{-# LANGUAGE NoMonomorphismRestriction #-}
module GivenTypeWriteFunction where

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = x

-- One version will typecheck
co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob x = bToc (aTob x)
-- 7. One version will typecheck.
a :: (a -> c) -> a -> a
a aToc x = x
-- 8. One version will typecheck.
a' :: (a -> b) -> a -> b
a' aTob x = aTob x

question = "\nYou will be shown a type and a function that needs to be written.\n" ++
            "Press space bar if you want to compare your thoughts to solution"

data Challenge = Challenge { function :: String
                           , func_sig :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { function = "There is only one version that works for\n" ++
                            "i :: a -> a"
                 , func_sig = "i x = x"
                 }
     , Challenge { function = "There is only one version that works for\n" ++
                            "c :: a -> b -> a"
                 , func_sig = "c x y = x"
                 }
     , Challenge { function = "Given alpha equivalence are c'' and c (last example) the same thing?\n" ++
                            "c'' :: b -> a -> b"
                 , func_sig = "Yes, they are the same!\n" ++
                            "c'' x y = x"
                 }
     , Challenge { function = "There is only one version that works for\n" ++
                            "c' :: a -> b -> b"
                 , func_sig = "c' x y = y"
                 }
     , Challenge { function = "There are multiple possibilities for\n" ++
                            "r :: [a] -> [a]"
                 , func_sig = "r x = x"
                 }
     , Challenge { function = "One version will typecheck\n" ++
                            "co :: (b -> c) -> (a -> b) -> a -> c"
                 , func_sig = "co bToc aTob x = bToc (aTob x)"
                 }
     , Challenge { function = "One version will typecheck\n" ++
                            "a :: (a -> c) -> a -> a"
                 , func_sig = "a aToc x = x"
                 }
     , Challenge { function = "One version will typecheck\n" ++
                            "a' :: (a -> b) -> a -> b"
                 , func_sig = "a' aTob x = aTob x"
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ question
                         , ""
                         , function c
                         , ""
                         ]

showResult :: Challenge -> String
showResult c = unlines [ ""
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
