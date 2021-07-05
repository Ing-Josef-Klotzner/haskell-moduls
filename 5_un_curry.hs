-- 5_un_curry.hs
module UnCurry where

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337
curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)
uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)
anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)
anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)
curry' f a b = f (a, b)
uncurry' f (a, b) = f a b

main = do
    putStrLn $ "nonsense :: Bool -> Integer" ++
            "\nnonsense True = 805" ++
            "\nnonsense False = 31337" ++
            "\n\ncurriedFunction :: Integer -> Bool -> Integer" ++
            "\ncurriedFunction i b = i + (nonsense b)" ++
            "\n\nuncurriedFunction :: (Integer, Bool) -> Integer" ++
            "\nuncurriedFunction (i, b) = i + (nonsense b)" ++
            "\n\nanonymous :: Integer -> Bool -> Integer" ++
            "\nanonymous = \\i b -> i + (nonsense b)" ++
            "\n\nanonNested :: Integer -> Bool -> Integer" ++
            "\nanonNested = \\i -> \\b -> i + (nonsense b)" ++
            "\n\ncurry f a b = f (a, b)" ++
            "\n\ncurriedFunction 10 False = " ++ show (curriedFunction 10 False) ++
            "\nuncurriedFunction (10, False) = " ++ show (uncurriedFunction (10, False)) ++
            "\nanonymous 10 False = " ++ show (anonymous 10 False) ++
            "\nanonNested 10 False = " ++ show (anonNested 10 False) ++
            "\ncurry' fst 13 28 = " ++ show (curry' fst 13 28) ++
            "\nuncurry' (+) (1, 2) = " ++ show (uncurry' (+) (1, 2))
