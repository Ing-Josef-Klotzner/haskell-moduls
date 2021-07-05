-- 17_ApplicativeExercise.hs
module ApplicativeExercise where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--List Applicative Exercise
--Implement the list Applicative. Writing a minimally complete
--Applicative instance calls for writing the definitions of both
--pure and <*>. We’re going to provide a hint as well. Use the
--checkers library to validate your Applicative instance.
data List a = Nil | Cons a (List a) deriving (Eq, Show)
--Remember what you wrote for the list Functor:
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a lis) = Cons (f a) (fmap f lis)
--Writing the list Applicative is similar.
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)
--Expected result:
--Prelude> let f = Cons (+1) (Cons (*2) Nil)
--Prelude> let v = Cons 1 (Cons 2 Nil)
--Prelude> f <*> v
--Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
--In case you get stuck, use the following functions and hints.
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)
concat' :: List (List a) -> List a
concat' = fold append Nil
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]
instance Eq a => EqProp (List a) where
  (=-=) = eq
listIntStrChar = Cons (([1], "a", 'c') :: ([Int], String, Char)) Nil
-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as


--ZipList Applicative Exercise
--Implement the ZipList Applicative. Use the checkers library to
--validate your Applicative instance. We’re going to provide
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a ls) = Cons a (take' (n-1) ls)
-- already defined above
--instance Functor List where
--    fmap = undefined
--instance Applicative List where
--    pure = undefined
--    (<*>) = undefined
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)
instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l
instance Functor ZipList' where
    fmap f (ZipList' a) = ZipList' $ fmap f a

instance Applicative ZipList' where
    pure a = ZipList' $ pure a
    _ <*> ZipList' Nil = ZipList' Nil
    ZipList' Nil <*> _ = ZipList' Nil
    ZipList' (Cons f Nil) <*> ZipList' (Cons g lis) = ZipList' $ Cons (f g) (f <$> lis)
    ZipList' (Cons f xs) <*> ZipList' (Cons g Nil) = ZipList' $ Cons (f g) (xs <*> pure g)
    ZipList' (Cons f xs) <*> ZipList' (Cons g lis) = ZipList' (Cons (f g) (xs <*> lis))

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a


--Exercise: Variations on Either
--Validation has the same representation as Either, but it can be
--different. The Functor will behave the same, but the Applicative
--will be different. See above for an idea of how Validation should
--behave. Use the checkers library.
data Validation e a = Failure' e | Success' a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' (f a)
-- This is different
instance Monoid e => Applicative (Validation e) where
    pure = Success'
    Failure' e <*> Failure' e' = Failure' $ mappend e e'
    Failure' e <*> _ = Failure' e
    _ <*> Failure' e = Failure' e
    Success' f <*> Success' a = Success' (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        a <- arbitrary
        e <- arbitrary
        oneof [return $ Failure' e, return $ Success' a]

genStringArray :: Gen [String]
genStringArray = do
    a <- arbitrary
    return [a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

type ValidationType = Validation [String] ([Int], [Int], [Int])

main :: IO ()
main = do
    putStrLn "\nList applicative tests"
    quickBatch $ applicative listIntStrChar

    putStrLn "\nZipList' applicative tests"
    quickBatch $ applicative (ZipList' listIntStrChar)

    putStrLn "\nValidation applicative tests"
    quickBatch $ applicative (Failure' ["xxx"] :: ValidationType)
