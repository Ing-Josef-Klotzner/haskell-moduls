-- 18_7_Chapter_Exercises.hs
module ChapterExercises18 where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (join)
import Control.Applicative ((<**>))
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
concat' :: List (List a) -> List a
concat' Nil = Nil
concat' (Cons Nil as) = concat' as
concat' (Cons a as) = append a (concat' as)
instance Monad List where
    Nil >>= _   = Nil
    as  >>= f   = concat' $ fmap f as
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
-- 1.
j :: Monad m => m (m a) -> m a
j x = x >>= id    --  = join
j' x = do
    y <- x
    y
--Expecting the following behavior:
--Prelude> j [[1, 2], [], [3]]
--[1,2,3]
--Prelude> j (Just (Just 1))
--Just 1
--Prelude> j (Just Nothing)
--Nothing
--Prelude> j Nothing
--Nothing

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = fmap f x
-- *ChapterExercises18> l1 (+1) [2,3,4]
--[3,4,5]
-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x = (<*>) (fmap f x)   -- = liftA2
l2' f x y =
    y >>= (\y'
        -> x >>= (\x'
            -> return $ f x' y'))
l2'' f x y = do
  a <- x
  b <- y
  return $ f a b
-- *ChapterExercises18> l2 (+) [2,3,4] [3,4,5]
--[5,6,7,6,7,8,7,8,9]
-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = l2 (\a f -> f a)   -- = <**>
a' x fy =
    x >>= (\a ->
        fy >>= (\f -> return $ f a))
a'' x fy = do
    a <- x
    f <- fy
    return $ f a
a''' x f = x <**> f
a'''' x f = f <*> x
-- *ChapterExercises18> a [1] [(+1)]
--[2]

-- 5.
-- You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f =
    f x >>= (\x' ->
        meh xs f >>= (\xs' -> return $ x':xs'))
meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] _ = return []
meh' (x:xs) f = do
    x' <- f x
    xs' <- meh' xs f
    return $ x':xs'
meh'' :: Monad m => [a] -> (a -> m b) -> m [b]
meh'' l f = traverse id (fmap f l)
meh''' :: Monad m => [a] -> (a -> m b) -> m [b]
meh''' = flip mapM
-- *ChapterExercises18> meh [1,2,3] (+) 5
--[6,7,8]
-- 6. Hint: reuse “meh”
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

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

