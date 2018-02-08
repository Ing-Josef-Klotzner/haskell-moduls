-- 5_Parametricity.hs
module Parametricity where

f1 :: a -> a -> a
f1 x y = return x y

f2 :: a -> a -> a
f2 x y = return y x

f3 :: a -> a -> a
f3 x y = (\_ -> x) y

f4 :: a -> a -> a
f4 x y = (\_ -> y) x

f5 :: a -> a -> a
f5 x y = const x y

f6 :: a -> a -> a
f6 x y = const y x

f7 :: a -> a -> a
f7 x = \x -> x

f8 :: a -> a -> a
f8 x = \_ -> x

f9 :: a -> a -> a
f9 x = id

f10 :: a -> a -> a
f10 x = const x

f11 :: a -> a -> a
f11 = f11

f12 :: a -> a -> a
f12 x y = x

f13 :: a -> a -> a
f13 x y = y

f14 :: a -> b -> b; 
f14 x y = y

main = do
    putStrLn $ "a function with type signature 'a -> a -> a' has two implementations, which are: " ++
            "\ni had no idea! :)"
    putStrLn $ "f1 8 6 = return 8 6\n" ++ show (f1 8 6)
    putStrLn $ "f2 8 6 = return 6 8\n" ++ show (f2 8 6)
    putStrLn $ "f3 8 6 = (\\_ -> 8) 6\n" ++ show (f3 8 6)
    putStrLn $ "f4 8 6 = (\\_ -> 6) 8\n" ++ show (f4 8 6)
    putStrLn $ "f5 8 6 = const 8 6\n" ++ show (f5 8 6)
    putStrLn $ "f6 8 6 = const 6 8\n" ++ show (f6 8 6)
    putStrLn $ "f7 8 6 = \\x -> x\n" ++ show (f7 8 6)
    putStrLn $ "f8 8 6 = \\_ -> x\n" ++ show (f8 8 6)
    putStrLn $ "f9 8 6 = id\n" ++ show (f9 8 6)
    putStrLn $ "f10 8 6 = const 8\n" ++ show (f10 8 6)
    putStrLn $ "f11 = f11  results in endless loop when apply f11 8 6"
    putStrLn $ "f12 8 6 = 8\n" ++ show (f12 8 6)
    putStrLn $ "f13 8 6 = 6\n" ++ show (f13 8 6)
    putStrLn "ups - there are 13 and more variants now"
    putStrLn $ "f14 :: a -> b -> b; f14 x y = y"
    putStrLn $ "f14 8 \"hi\"\n" ++ show (f14 8 "hi")

