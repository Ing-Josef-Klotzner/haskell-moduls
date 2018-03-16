-- 18_7_Chapter_Exercises.hs
module ChapterExercises18 where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--18.7 Chapter Exercises
--Write Monad instances for the following types. Use the QuickCheck
--properties we showed you to validate your instances.

-- 1. Welcome to the Nope Monad, where nothing happens and
--nobody cares.
data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor (Nope) where
    fmap f NopeDotJpg = NopeDotJpg
instance Applicative (Nope) where
    pure x = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg
instance Monad (Nope) where
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg
instance EqProp (Nope a) where
    (=-=) = eq
instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg
type STuple = (String, String, String)
-- We're serious. Write it anyway.
-- 2.
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
    fmap f (Left' a) = Left' (f a)
    fmap f (Right' b) = Right' b
instance Monoid b => Applicative (PhhhbbtttEither b) where
    pure = Left'
    (Right' x) <*> (Right' y) = Right' (x `mappend` y)
    (Right' x) <*> _ = Right' x
    _ <*> (Right' x) = Right' x
    (Left' x) <*> (Left' y) = Left' (x y)
instance Monoid b => Monad (PhhhbbtttEither b) where
    return = pure
    Right' x >>= f = Right' x
    Left' x >>= f = f x
instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Left' a, return $ Right' b]
instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

-- 3. Write a Monad instance for Identity.
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
    pure = Identity
    Identity x <*> Identity y = Identity (x y) 
instance Monad Identity where
    return = pure
    Identity x >>= f = f x
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a
instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- 4. This one should be easier than the Applicative instance
--was. Remember to use the Functor that Monad requires, then
--see where the chips fall.
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a lis) = Cons (f a) (fmap f lis)
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys
instance Applicative List  where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons a as) <*> xs = append (a <$> xs) (as <*> xs)
instance Monad List where
    return = pure
    Nil >>= _ = Nil
    Cons a as >>= f = f a
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Cons a (Cons b Nil), Nil]
instance Eq a => EqProp (List a) where
    (=-=) = eq

--Write the following functions using the methods provided
--by Monad and Functor. Using stuff like identity and composition
--is fine, but it has to typecheck with types provided.

main :: IO ()
main = do
    putStrLn $ "\nquickBatch $ functor (NopeDotJpg :: Nope STuple)"
    quickBatch $ functor (NopeDotJpg :: Nope STuple)
    putStrLn $ "\nquickBatch $ applicative (NopeDotJpg :: Nope STuple)"
    quickBatch $ applicative (NopeDotJpg :: Nope STuple)
    putStrLn $ "\nquickBatch $ monad (NopeDotJpg :: Nope STuple)"
    quickBatch $ monad (NopeDotJpg :: Nope STuple)
    putStrLn $ "\nquickBatch monad $ (Right' (\"a\", \"a\", \"a\") :: PhhhbbtttEither STuple STuple)"
    quickBatch $ monad (Right' ("a", "a", "a") :: PhhhbbtttEither STuple STuple)
    putStrLn $ "\nquickBatch monad $ (Identity (\"a\", \"a\", \"a\") :: Identity STuple)"
    quickBatch $ monad (Identity ("a", "a", "a") :: Identity STuple)
    putStrLn $ "\nquickBatch $ functor (Cons (\"a\", \"a\", \"a\") Nil :: List STuple)"
    quickBatch $ functor (Cons ("a", "a", "a") Nil :: List STuple)
    putStrLn $ "\nquickBatch $ applicative (Cons (\"a\", \"a\", \"a\") Nil :: List STuple)"
    quickBatch $ applicative (Cons ("a", "a", "a") Nil :: List STuple)
    putStrLn $ "\nquickBatch $ monad (Cons (\"a\", \"a\", \"a\") Nil :: List STuple)"
    quickBatch $ monad (Cons ("a", "a", "a") Nil :: List STuple)

