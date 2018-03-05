--15_Exercise_Semigroup.hs
-- wird nur für Roberts Variante gebraucht: (nächste 2 Zeilen)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Exercise_Semigroup where
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck
import Data.Monoid (Monoid, Sum(..))

--15.15 Chapter exercises
--Semigroup exercises
--Given a datatype, implement the Semigroup instance. Add
--Semigroup constraints to type variables where needed. Use the
--Semigroup class from the semigroups library (or from base if you
--are on GHC 8) or write your own. When we use (<>), we mean
--the infix mappend from the Semigroup typeclass.
--Note We’re not always going to derive every instance you
--may want or need in the datatypes we provide for exercises.
--We expect you to know what you need and to take care of it
--yourself by this point.

--1. Validate all of your instances with QuickCheck. Since
--Semigroup’s only law is associativity, that’s the only prop-
--erty you need to reuse. Keep in mind that you’ll poten-
--tially need to import the modules for Monoid and Semigroup
--and to avoid naming conflicts for the (<>) depending on
--your version of GHC.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- > semigroupAssoc Trivial Trivial Trivial
--True

-- 2.
newtype Identity a = Identity a

type S = String
type Id = Identity
type IdentityAssoc = Id S -> Id S -> Id S -> Bool

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity a' = Identity (a <> a')

instance Eq a => Eq (Identity a) where
    Identity a == Identity a' = a == a'

testIdEq :: Eq a => Identity a -> Identity a -> Bool
testIdEq x x' = (x == x') == (\(Identity a) (Identity b) -> a == b) x x'

type IdentityEq = Id S -> Id S -> Bool

instance (Show a) => Show (Identity a) where
    show (Identity a) = "Identity " ++ show a

testIdShow :: Show a => Identity a -> Bool
testIdShow x = show x == (\(Identity a) -> "Identity " ++ show a) x

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId


-- 3.
data Two a b = Two a b
--Hint: Ask for another Semigroup instance.
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')
instance (Eq a, Eq b) => Eq (Two a b) where
    Two a b == Two a' b' = (a == a') && (b == b')
instance (Show a, Show b) => Show (Two a b) where
    show (Two a b) = "Two " ++ show a ++ " " ++ show b
type TwoStringAssoc = Two S S -> Two S S -> Two S S -> Bool 

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

-- 4.
data Three a b c = Three a b c
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')
instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where
    Three a b c == Three a' b' c' = (a == a') && (b == b') && (c == c')
instance (Show a, Show b, Show c) => Show (Three a b c) where
    show (Three a b c) = "Three " ++ show a ++ " " ++ show b ++ " " ++ show c
type ThreeStringAssoc = Three S S S -> Three S S S -> Three S S S -> Bool

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = genThree

-- 5.
data Four a b c d = Four a b c d
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')
instance (Eq a, Eq b, Eq c, Eq d) => Eq (Four a b c d) where
    Four a b c d == Four a' b' c' d' = (a == a') && (b == b') && (c == c') && (d == d')
instance (Show a, Show b, Show c, Show d) => Show (Four a b c d) where
    show (Four a b c d) = "Four " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d
type FourStringAssoc = Four S S S S -> Four S S S S -> Four S S S S -> Bool

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = genFour

-- 6.
newtype BoolConj = BoolConj Bool
--What it should do:
--Prelude> (BoolConj True) <> (BoolConj True)
--BoolConj True
--Prelude> (BoolConj True) <> (BoolConj False)
--BoolConj False
instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    BoolConj _ <> BoolConj _ = BoolConj False
instance Show BoolConj where
    show (BoolConj a) = "BoolConj " ++ show a
instance Eq BoolConj where
    BoolConj a == BoolConj b = a == b
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
-- testing expected behaviour
propBoolConj :: BoolConj -> BoolConj -> Bool
propBoolConj (BoolConj a) (BoolConj b) = BoolConj a <> BoolConj b == BoolConj (a && b)

genBoolC :: Gen BoolConj
genBoolC = do
    x <- arbitrary :: Gen Bool
    return $ BoolConj x

instance Arbitrary BoolConj where
    arbitrary = genBoolC

-- 7.
newtype BoolDisj = BoolDisj Bool
--What it should do:
--Prelude> (BoolDisj True) <> (BoolDisj True)
--BoolDisj True
--Prelude> (BoolDisj True) <> (BoolDisj False)
--BoolDisj True
instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    BoolDisj _ <> BoolDisj _ = BoolDisj True
instance Show BoolDisj where
    show (BoolDisj a) = "BoolDisj " ++ show a
instance Eq BoolDisj where
    BoolDisj a == BoolDisj b = a == b
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
-- testing expected behaviour
propBoolDisj :: BoolDisj -> BoolDisj -> Bool
propBoolDisj (BoolDisj a) (BoolDisj b) = BoolDisj a <> BoolDisj b == BoolDisj (a || b)
genBoolD :: Gen BoolDisj
genBoolD = do
    x <- arbitrary :: Gen Bool
    return $ BoolDisj x

instance Arbitrary BoolDisj where
    arbitrary = genBoolD

-- 8.
data Or a b = Fst a | Snd b
--The Semigroup for Or should have the following behavior.
--We can think of this as having a “sticky” Snd value where
--it’ll hold onto the first Snd value when and if one is passed
--as an argument. This is similar to the First' Monoid you
--wrote earlier.
--Prelude> Fst 1 <> Snd 2
--Snd 2
--Prelude> Fst 1 <> Fst 2
--Fst 2
--Prelude> Snd 1 <> Fst 2
--Snd 1
--Prelude> Snd 1 <> Snd 2
--Snd 1
instance Semigroup (Or a b) where
    Snd x <> _ = Snd x
    Fst _ <> y = y

instance (Eq a, Eq b) => Eq (Or a b) where
    Fst x == Fst x' = (x == x')
    Snd y == Snd y' = (y == y')

instance (Show a, Show b) => Show (Or a b) where
    show (Fst x) = "Fst " ++ show x
    show (Snd y) = "Snd " ++ show y

propOr :: (Eq a, Eq b) => Or a b -> Or a b -> Bool
propOr x x' = case (x, x') of
    (Fst _, Fst b) -> x <> x' == Fst b
    (Fst _, Snd b) -> x <> x' == Snd b
    (Snd b, _) -> x <> x' == Snd b

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary 
  b <- arbitrary
  elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

-- 9.
newtype Combine a b = Combine { unCombine :: (a -> b) }
--What it should do:
--Prelude> let f = Combine $ \n -> Sum (n + 1)
--Prelude> let g = Combine $ \n -> Sum (n - 1)
--Prelude> unCombine (f <> g) $ 0
--Sum {getSum = 0}
--Prelude> unCombine (f <> g) $ 1
--Sum {getSum = 2}
--Prelude> unCombine (f <> f) $ 1
--Sum {getSum = 4}
--Prelude> unCombine (g <> f) $ 1
--Sum {getSum = 2}
--Hint: This function will eventually be applied to a single
--value of type a. But you’ll have multiple functions that can
--produce a value of type b. How do we combine multiple
--values so we have a single b? This one will probably be
--tricky! Remember that the type of the value inside of
--Combine is that of a function. The type of functions should
--already have an Arbitrary instance that you can reuse for
--testing this instance.
instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)
instance Show (Combine a b) where
    show (f) = "Combine " ++ "<function>"
--    show (unCombine (Combine f)) = "<function>"
-- Prelude> let h = Combine $ \n -> Sum (n + 2)
-- unCombine (f <> (g <> h)) $ 1 == unCombine ((f <> g) <> h) $ 1
propCombine :: (Eq b, Semigroup b) => a -> Combine a b -> Combine a b -> Combine a b -> Bool
propCombine x f g h = (unCombine (f <> (g <> h))) x == (unCombine ((f <> g) <> h)) x

newtype PlusInt = PlusInt Int deriving (Show, CoArbitrary, Arbitrary, Eq)

instance Semigroup PlusInt where
  PlusInt a <> PlusInt b = PlusInt $ a + b

-- variant Robert of testing Combine
testMonoid :: Gen Bool
testMonoid = do
    f1 :: (Combine Int PlusInt) <- arbitrary
    f2 <- arbitrary
    f3 <- arbitrary
    x :: Int  <- arbitrary
    pure $ unCombine (f1 <> f2 <> f3) x  == unCombine (f1 <> (f2 <> f3)) x

type CombineAssoc a b = a -> Combine a b -> Combine a b -> Combine a b -> Bool

-- not used
genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
genFunc = arbitrary

instance Show (a -> b) where
    show f = "<function>"

-- not used
genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCombine = do
    f <- genFunc
    return $ Combine f

--instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--    arbitrary = genCombine

-- in ghci
-- *Exercise_Semigroup> let f = Combine $ \n -> Sum (n + 2)
-- *Exercise_Semigroup> let g = Combine $ \n -> Sum (n + 1)
-- *Exercise_Semigroup> let h = Combine $ \n -> Sum (n + 4)
-- *Exercise_Semigroup> propCombine 3 f g h
--True

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary   --(equivalent to   fmap Combine arbitrary)

-- 10.
newtype Comp a = Comp { unComp :: (a -> a) }
--Hint: We can do something that seems a little more spe-
--cific and natural to functions now that the input and out-
--put types are the same.
instance Semigroup (Comp a) where
    Comp f <> Comp g = Comp (f . g)
instance Show (Comp a) where
    show (f) = "Comp " ++ "<function>"
instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary   --(equivalent to   fmap Comp arbitrary)
type CompAssoc a = a -> Comp a -> Comp a -> Comp a -> Bool
propComp :: (Eq a, Semigroup a) => a -> Comp a -> Comp a -> Comp a -> Bool
propComp x f g h = (unComp (f <> (g <> h))) x == (unComp ((f <> g) <> h)) x

-- 11.
-- Look familiar?
data Validation a b = Failure' a | Success' b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
    Success' x <> _ = Success' x
    _ <> Success' x = Success' x
    Failure' a <> Failure' b = Failure' (a <> b)
--Given this code:
--main = do
--    let failure :: String -> Validation String Int
--        failure = Failure
--    success :: Int -> Validation String Int
--    success = Success
--    print $ success 1 <> failure "blah"
--    print $ failure "woot" <> failure "blah"
--    print $ success 1 <> success 2
--    print $ failure "woot" <> success 2
--You should get this output:
--Prelude> main
--Success 1
--Failure "wootblah"
--Success 1
--Success 2


--                 Monoid exercises
--Given a datatype, implement the Monoid instance. Add Monoid
--constraints to type variables where needed. For the datatypes
--you’ve already implemented Semigroup instances for, you need
--to figure out what the identity value is.
--1. Again, validate all of your instances with QuickCheck.
--Example scaffold is provided for the Trivial type.

-- already definded in semigroup section:
--data Trivial = Trivial deriving (Eq, Show)
--instance Semigroup Trivial where
--    _ <> _ = Trivial
--type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mappend mempty m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = mappend m mempty == m

-- 2.
-- already definded in semigroup section:
--newtype Identity a = Identity a deriving Show
instance (Semigroup a, Monoid a) => Monoid (Identity a) where 
    mempty = Identity mempty
    mappend = (<>)

-- 3.
-- already definded in semigroup section:
-- data Two a b = Two a b deriving Show
instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty = Two mempty mempty
    mappend = (<>)

-- 4.
-- already definded in semigroup section:
-- newtype BoolConj = BoolConj Bool
--What it should do:
--Prelude> (BoolConj True) `mappend` mempty
--BoolConj True
--Prelude> mempty `mappend` (BoolConj False)
--BoolConj False
instance Monoid (BoolConj) where 
    mempty = BoolConj True
    mappend = (<>)

-- 5.
-- already definded in semigroup section:
-- newtype BoolDisj = BoolDisj Bool
--What it should do:
--Prelude> (BoolDisj True) `mappend` mempty
--BoolDisj True
--Prelude> mempty `mappend` (BoolDisj False)
--BoolDisj False
instance Monoid (BoolDisj) where 
    mempty = BoolDisj False
    mappend = (<>)

-- 6.
-- already definded in semigroup section:
-- newtype Combine a b = Combine { unCombine :: (a -> b) }
--What it should do:
--Prelude> let f = Combine $ \n -> Sum (n + 1)
--Prelude> unCombine (mappend f mempty) $ 1
--Sum {getSum = 2}
instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine mempty
    mappend = (<>)

instance Monoid PlusInt where
    mempty = PlusInt 0
    PlusInt a `mappend` PlusInt b = PlusInt $ a + b

-- > (unCombine (mappend f mempty) $ 1) == (unCombine (mappend mempty f) $ 1)
--True
monoidIdentity :: (Eq b, Monoid b, Semigroup b) => Combine a b -> a -> Bool
monoidIdentity f x = (unCombine (mappend f mempty) $ x) == (unCombine (mappend mempty f) $ x)

-- 7. Hint: We can do something that seems a little more spe-
--cific and natural to functions now that the input and out-
--put types are the same.
-- already definded in semigroup section:
-- newtype Comp a = Comp (a -> a)
instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

monoidCpIdentity :: (Eq a) => Comp a -> a -> Bool
monoidCpIdentity f x = (unComp (f <> mempty) $ x) == (unComp (mempty <> f) $ x)

--8. This next exercise will involve doing something that will
--feel a bit unnatural still and you may find it difficult. If you
--get it and you haven’t done much FP or Haskell before,
--get yourself a nice beverage. We’re going to toss you
--the instance declaration so you don’t churn on a missing
--Monoid constraint you didn’t know you needed.
newtype Mem s a = 
    Mem {
        runMem :: s -> (a,s)
    }
instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem (\x ->
    let (a, b) = g x
        -- apply f to the b result of g x
        (c, d) = f b
    -- return the new tuple with the first constituents combined
    in (a <> c, d))
instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)

instance Show (Mem a b) where
    show (f) = "Mem " ++ "<function>"

memIdentity :: (Eq a, Eq b, Monoid b, Semigroup b) => Mem a b -> a -> Bool
memIdentity f x = (runMem (f <> mempty) $ x) == (runMem (mempty <> f) $ x)

instance (CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Mem a b) where
  arbitrary = Mem <$> arbitrary   --(equivalent to   fmap Combine arbitrary)

--Given the following code:
f' = Mem $ \s -> ("hi", s + 1)

--main = do
--    let rmzero = runMem mempty 0
--    rmleft = runMem (f' <> mempty) 0
--    rmright = runMem (mempty <> f') 0
--    print $ rmleft
--    print $ rmright
--    print $ (rmzero :: (String, Int))
--    print $ rmleft == runMem f' 0
--    print $ rmright == runMem f' 0

--A correct Monoid for Mem should, given the above code, get
--the following output:
--Prelude> main
--("hi",1)
--("hi",1)
--("",0)
--True
--True

--Make certain your instance has output like the above, this
--is sanity-checking the Monoid identity laws for you! It’s not
--a proof and it’s not even as good as property testing, but
--it’ll catch the most common mistakes people make.

--It’s not a trick and you don’t need a Monoid for s. Yes, such
--a Monoid can and does exist. Hint: chain the s values from
--one function to the other. You’ll want to check the identity
--laws as a common first attempt will break them.


main :: IO ()
main = do
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool"
    quickCheck (semigroupAssoc :: TrivAssoc)
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool"
    quickCheck (semigroupAssoc :: IdentityAssoc)
    putStrLn "Testing   testIdEq x x' = (x == x') == (\\(Identity a) (Identity b) -> a == b) x x'   ..."
    quickCheck (testIdEq :: IdentityEq)
    putStrLn "Testing   testIdShow x = show x == (\\(Identity a) -> \"Identity \" ++ show a) x   ..."
    quickCheck (testIdShow :: Id String -> Bool)
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type TwoStringAssoc = Two S S -> Two S S -> Two S S -> Bool"
    quickCheck (semigroupAssoc :: TwoStringAssoc)
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type ThreeStringAssoc = Three S S S -> Three S S S -> Three S S S -> Bool"
    quickCheck (semigroupAssoc :: ThreeStringAssoc)
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type FourStringAssoc = Four S S S S -> Four S S S S -> Four S S S S -> Bool"
    quickCheck (semigroupAssoc :: FourStringAssoc)
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool"
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    putStrLn $ "Testing   propBoolConj (BoolConj a) (BoolConj b) = \n"
            ++ "                        BoolConj a <> BoolConj b == BoolConj (a && b)   ..."
    putStrLn "with   propBoolConj :: BoolConj -> BoolConj -> Bool"
    quickCheck propBoolConj
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool"
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    putStrLn $ "Testing   propBoolDisj (BoolDisj a) (BoolDisj b) = \n"
            ++ "                        BoolDisj a <> BoolDisj b == BoolDisj (a || b)   ..."
    putStrLn "with   propBoolDisj :: BoolDisj -> BoolDisj -> Bool"
    quickCheck propBoolDisj
    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
    putStrLn "with   type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool"
    quickCheck (semigroupAssoc :: OrAssoc)
    putStrLn $ "Testing   propOr x x' = case (x, x') of\n" ++
            "                (Fst _, Fst b) -> x <> x' == Fst b\n" ++
            "                (Fst _, Snd b) -> x <> x' == Snd b\n" ++
            "                (Snd b, _) -> x <> x' == Snd b   ..."
    putStrLn "with   propOr :: (Eq a, Eq b) => Or Int Int -> Or Int Int -> Bool"
    quickCheck (propOr :: Or Int Int -> Or Int Int -> Bool)
    putStrLn "with   propOr :: (Eq a, Eq b) => Or String String -> Or String String -> Bool"
    quickCheck (propOr :: Or S S -> Or S S -> Bool)
    putStrLn "with   propOr :: (Eq a, Eq b) => Or Bool Bool -> Or Bool Bool -> Bool"
    quickCheck (propOr :: Or Bool Bool -> Or Bool Bool -> Bool)

    putStrLn "Testing   propCombine x f g h = (unCombine (f <> (g <> h))) x == (unCombine ((f <> g) <> h)) x   ..."
    putStrLn "with propCombine :: Int -> (Combine Int PlusInt) -> (Combine Int PlusInt) -> (Combine Int PlusInt) -> Bool"
    quickCheck (propCombine :: Int -> (Combine Int PlusInt) -> (Combine Int PlusInt) -> (Combine Int PlusInt) -> Bool)
    -- oder andere Variante:
    putStrLn "Testing   propCombine x f g h = (unCombine (f <> (g <> h))) x == (unCombine ((f <> g) <> h)) x   ..."
    putStrLn "with propCombine :: CombineAssoc Int PlusInt"
    quickCheck (propCombine :: CombineAssoc Int PlusInt)
-- Roberts Variante - was da genau passiert? - liefert keinen output mit verbose ?!?!
    putStrLn "Testing   testMonoid   (Variante Robert of testing Combine)   ..."
    quickCheck testMonoid
    putStrLn "Testing   propComp x f g h = (unComp (f <> (g <> h))) x == (unComp ((f <> g) <> h)) x   ..."
    putStrLn "with propComp :: CompAssoc PlusInt"
    quickCheck (propComp :: CompAssoc PlusInt)

    let failure :: String -> Validation String Int
        failure = Failure'
        success :: Int -> Validation String Int
        success = Success'
    putStrLn $ "\n" ++
        " >let failure :: String -> Validation String Int\n" ++
        " >    failure = Failure'\n" ++
        " >    success :: Int -> Validation String Int\n" ++
        " >    success = Success'"
    putStrLn $ " >success 1 <> failure \"blah\"\n" ++ show (success 1 <> failure "blah")
    putStrLn $ " >failure \"woot\" <> failure \"blah\"\n" ++ show (failure "woot" <> failure "blah")
    putStrLn $ " >success 1 <> success 2\n" ++ show (success 1 <> success 2)
    putStrLn $ " >failure \"woot\" <> success 2\n" ++ show (failure "woot" <> success 2)

-- Monoid Exercises:

-- already tested above when testing semigroup only
--    putStrLn "Testing   semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)   ..."
--    putStrLn "with   type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool"
--    quickCheck (semigroupAssoc :: TrivAssoc)
    putStrLn "\nTesting   monoidLeftIdentity m = mappend mempty m == m   ..."
    putStrLn "with   type Trivial -> Bool"
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    putStrLn "Testing   monoidRightIdentity m = mappend m mempty == m   ..."
    putStrLn "with   type Trivial -> Bool"
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)

    putStrLn "Testing   monoidLeftIdentity m = mappend mempty m == m   ..."
    putStrLn "with   type Identity String -> Bool"
    quickCheck (monoidLeftIdentity :: Id S -> Bool)
    putStrLn "Testing   monoidRightIdentity m = mappend m mempty == m   ..."
    putStrLn "with   type Identity String -> Bool"
    quickCheck (monoidLeftIdentity :: Id S -> Bool)

    putStrLn "Testing   monoidLeftIdentity m = mappend mempty m == m   ..."
    putStrLn "with   type Two String String -> Bool"
    quickCheck (monoidLeftIdentity :: Two S S -> Bool)
    putStrLn "Testing   monoidRightIdentity m = mappend m mempty == m   ..."
    putStrLn "with   type Two String String -> Bool"
    quickCheck (monoidLeftIdentity :: Two S S -> Bool)

    putStrLn "Testing   monoidLeftIdentity m = mappend mempty m == m   ..."
    putStrLn "with   type BoolConj -> Bool"
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    putStrLn "Testing   monoidRightIdentity m = mappend m mempty == m   ..."
    putStrLn "with   type BoolConj -> Bool"
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)

    putStrLn "Testing   monoidLeftIdentity m = mappend mempty m == m   ..."
    putStrLn "with   type BoolDisj -> Bool"
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    putStrLn "Testing   monoidRightIdentity m = mappend m mempty == m   ..."
    putStrLn "with   type BoolDisj -> Bool"
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)

    putStrLn "Testing   monoidIdentity f x = (unCombine (f <> mempty) $ x) == (unCombine (mempty <> f) $ x)   ..."
    putStrLn "with   type Combine Int PlusInt -> Int -> Bool"
    quickCheck (monoidIdentity :: Combine Int PlusInt -> Int -> Bool)

    putStrLn "Testing   monoidCpIdentity f x = (unComp (f <> mempty) $ x) == (unComp (mempty <> f) $ x)   ..."
    putStrLn "with   type Comp PlusInt -> PlusInt -> Bool"
    quickCheck (monoidCpIdentity :: Comp PlusInt -> PlusInt -> Bool)
    
    putStrLn "Testing   monoidCpIdentity f x = (unComp (f <> mempty) $ x) == (unComp (mempty <> f) $ x)   ..."
    putStrLn "with   type Comp Int -> Int -> Bool"
    quickCheck (monoidCpIdentity :: Comp Int -> Int -> Bool)

    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    putStrLn $ "\n >runMem (f' <> mempty) 0\n" ++ show rmleft
    putStrLn $ " >runMem (mempty <> f') 0\n" ++ show rmright
    putStrLn $ " >runMem mempty 0\n" ++ show (rmzero :: (String, Int))
    putStrLn $ " >runMem (f' <> mempty) 0 == runMem f' 0\n" ++ show (rmleft == runMem f' 0)
    putStrLn $ " >runMem (mempty <> f') 0 == runMem f' 0\n" ++ show (rmright == runMem f' 0)

    putStrLn "\nTesting   memIdentity f x = (runMem (f <> mempty) $ x) == (runMem (mempty <> f) $ x)   ..."
    putStrLn "with   type Mem Int PlusInt -> Int -> Bool"
    quickCheck (memIdentity :: Mem Int PlusInt -> Int -> Bool)

