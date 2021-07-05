-- 16_Functor.hs
module Functor where

data FixMePls a = FixMe | Pls a deriving (Eq, Show)
instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt | Matter a  | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)

data CountingGood a = Heisenberg Int a deriving (Eq, Show)
-- Totes cool.
instance Functor CountingGood where
    fmap f (Heisenberg n a) = Heisenberg (n) (f a)

j = (++ " Jesse")
l = (++ " lol")
u = "Uncle"
oneWhoKnocks = Heisenberg 0 u

replaceWithP :: b -> Char
replaceWithP = const 'p'
lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]
-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP
--What happens if we lift it?
-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f
--                  => f a -> f Char
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP
-- But we can assert a more specific type for liftedReplace!
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
-- :: (Functor f1, Functor f)
-- => f (f1 a) -> f (f1 Char)
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP
-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f ~ []
-- f1 ~ Maybe

-- Prelude> let rWP = replaceWithP
-- Prelude> :t (fmap . fmap . fmap) rWP
-- (fmap . fmap . fmap) replaceWithP
-- :: (Functor f2, Functor f1, Functor f)
-- => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP
-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f ~ []
-- f1 ~ Maybe
-- f2 ~ []


main :: IO ()
main = do
    putStrLn $ " *>fmap (+1) (Pls 1)\n" ++ show (fmap (+1) (Pls 1))
    putStrLn $ " *>fmap id (Pls 1)\n" ++ show (fmap id (Pls 1))
    putStrLn $ " *>fmap ((+1) . (*2)) [1..5]\n" ++ show (fmap ((+1) . (*2)) [1..5])
    putStrLn $ " *>fmap (+1) . fmap (*2) $ [1..5]\n" ++ show (fmap (+1) . fmap (*2) $ [1..5])
    
    putStrLn $ " *>fmap j . fmap l $ oneWhoKnocks\n" ++ show (fmap j . fmap l $ oneWhoKnocks)
    putStrLn $ " *>fmap (j . l) $ oneWhoKnocks\n" ++ show (fmap (j . l) $ oneWhoKnocks)

    putStrLn $ " *>fmap replaceWithP (Just 10)\n" ++ show (fmap replaceWithP (Just 10))
    putStrLn $ " *>fmap replaceWithP (10, 20)\n" ++ show (fmap replaceWithP (10, 20))
    putStrLn $ " *>replaceWithP [Just \"Ave\", Just \"woohoo\", Nothing]\n" ++ show (replaceWithP [Just "Ave", Just "woohoo", Nothing])
    putStrLn $ " *>fmap replaceWithP [Just \"Ave\", Just \"woohoo\", Nothing]\n" ++ show (fmap replaceWithP [Just "Ave", Just "woohoo", Nothing])
    putStrLn $ " *>(fmap . fmap) replaceWithP [Just \"Ave\", Just \"woohoo\", Nothing]\n" ++ show ((fmap . fmap) replaceWithP [Just "Ave", Just "woohoo", Nothing])
    putStrLn $ " *>(fmap . fmap . fmap) replaceWithP [Just \"Ave\", Just \"woohoo\", Nothing]\n" ++ show ((fmap . fmap . fmap) replaceWithP [Just "Ave", Just "woohoo", Nothing])
    
    putStr "replaceWithP' lms:  "
    print (replaceWithP' lms)
    putStr "liftedReplace lms:  "
    print (liftedReplace lms)
    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)
    putStr "twiceLifted lms:    "
    print (twiceLifted lms)
    putStr "twiceLifted' lms:   "
    print (twiceLifted' lms)
    putStr "thriceLifted lms:   "
    print (thriceLifted lms)
    putStr "thriceLifted' lms:  "
    print (thriceLifted' lms)
