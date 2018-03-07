--16_10_Exercises_Func_Instances.hs
module FuncInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

--Implement Functor instances for the following datatypes. Use
--the QuickCheck properties we showed you to validate them.

-- 1.
newtype Identity a = Identity a
instance Functor (Identity) where
    fmap f (Identity x) = Identity (f x)

instance Eq a => Eq (Identity a) where
    Identity x == Identity x' = x == x'

instance (Show a) => Show (Identity a) where
    show (Identity a) = "Identity " ++ show a

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

type IntListId = Identity [Int] -> Bool
type IntListCmpsId = [Int] -> Fun Int Int -> Fun Int (Identity Int) -> Bool

propIdent :: Identity [Int] -> Bool
propIdent x = functorIdentity (Identity x)


-- 2.
data Pair a = Pair a a
instance Functor (Pair) where
    fmap f (Pair x y) = Pair (f x) (f y)
instance Eq a => Eq (Pair a) where
    Pair x y == Pair x' y' = (x == x') && (y == y')
instance Show a => Show (Pair a) where
    show (Pair x y) = "Pair " ++ show x ++ " " ++ show y
type IntListPair = Pair [Int] -> Bool
type IntListCmpsPair = [Int] -> Fun Int Int -> Fun Int (Pair Int) -> Bool

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = genPair

-- 3.
data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
instance (Eq a, Eq b) => Eq (Two a b) where
    Two a b == Two a' b' = (a == a') && (b == b')
instance (Show a, Show b) => Show (Two a b) where
    show (Two a b) = "Two " ++ show a ++ " " ++ show b
type IntListTwo = Two [Int] [Int] -> Bool
type IntListCmpsTwo = [Int] -> Fun Int Int -> Fun Int (Two Int Int) -> Bool

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

-- 4.
data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where
    Three a b c == Three a' b' c' = (a == a') && (b == b') && (c == c')
instance (Show a, Show b, Show c) => Show (Three a b c) where
    show (Three a b c) = "Three " ++ show a ++ " " ++ show b ++ " " ++ show c
type IntListThree = Three [Int] [Int] [Int] -> Bool
type IntListCmpsThree = [Int] -> Fun Int Int -> Fun Int (Three Int Int Int) -> Bool

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = genThree

-- 5.
data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)
instance (Eq a, Eq b) => Eq (Three' a b) where
    Three' a b c == Three' a' b' c' = (a == a') && (b == b') && (c == c')
instance (Show a, Show b) => Show (Three' a b) where
    show (Three' a b c) = "Three' " ++ show a ++ " " ++ show b ++ " " ++ show c
type IntListThree' = Three' [Int] [Int] -> Bool
type IntListCmpsThree' = [Int] -> Fun Int Int -> Fun Int (Three' Int Int) -> Bool

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = genThree'

--6.
data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
instance (Eq a, Eq b, Eq c, Eq d) => Eq (Four a b c d) where
    Four a b c d == Four a' b' c' d' = (a == a') && (b == b') && (c == c') && (d == d')
instance (Show a, Show b, Show c, Show d) => Show (Four a b c d) where
    show (Four a b c d) = "Four " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
type IntListFour = Four [Int] [Int] [Int] [Int] -> Bool
type IntListCmpsFour = [Int] -> Fun Int Int -> Fun Int (Four Int Int Int Int) -> Bool

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = genFour

--7.
data Four' a b = Four' a a a b
instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
instance (Eq a, Eq b) => Eq (Four' a b) where
    Four' a b c d == Four' a' b' c' d' = (a == a') && (b == b') && (c == c') && (d == d')
instance (Show a, Show b) => Show (Four' a b) where
    show (Four' a b c d) = "Four' " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
type IntListFour' = Four' [Int] [Int] -> Bool
type IntListCmpsFour' = [Int] -> Fun Int Int -> Fun Int (Four' Int Int) -> Bool

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = genFour'

--8.
--Can you implement one for this type? Why? Why not?
data Trivial = Trivial
-- Answer: no, it can't be applied on type constants'

main :: IO ()
main = do
    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Identity [Int] -> Bool"
    quickCheck (functorIdentity :: IntListId)
    putStrLn " Testing same with created instance propIdent"
    quickCheck propIdent
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Identity Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsId)

    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Pair [Int] -> Bool"
    quickCheck (functorIdentity :: IntListPair)
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Pair Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsPair)

    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Two [Int] [Int] -> Bool"
    quickCheck (functorIdentity :: IntListTwo)
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Two Int Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsTwo)

    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Three [Int] [Int] [Int] -> Bool"
    quickCheck (functorIdentity :: IntListThree)
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Three Int Int Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsThree)

    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Three' [Int] [Int] -> Bool"
    quickCheck (functorIdentity :: IntListThree')
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Three' Int Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsThree')

    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Four [Int] [Int] [Int] [Int] -> Bool"
    quickCheck (functorIdentity :: IntListFour)
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Four Int Int Int Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsFour)

    putStrLn $ "Testing   functorIdentity f = fmap id f == f\n" ++
                "with type   Four' [Int] [Int] -> Bool"
    quickCheck (functorIdentity :: IntListFour')
    putStrLn $ "Testing   functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)\n" ++
                "with type   [Int] -> Fun Int Int -> Fun Int (Four' Int Int) -> Bool"
    quickCheck (functorCompose' :: IntListCmpsFour')

