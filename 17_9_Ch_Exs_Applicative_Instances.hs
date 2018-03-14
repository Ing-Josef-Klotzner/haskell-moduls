--17_9_Ch_Exs_Applicative_Instances.hs
module ApplicativeInstances where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--Write instances for the following datatypes. Confused?
--Write out what the type should be. Use the checkers library
--to validate the instances.
--  1.
data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)
instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)
instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        pure $ Pair a b
instance Eq a => EqProp (Pair a) where
    (=-=) = eq
-- *ApplicativeInstances> Pair ((++"hi")) ((++"udo")) <*> (Pair ("a") ("d"))
--Pair "ahi" "dudo"
--  2. This should look familiar.
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b
    Two f g <*> Two a b = Two (f `mappend` a) (g b)
-- *ApplicativeInstances> Two (("hi")) ((++"udo")) <*> (Two ("a") ("d"))
--Two "a" "dudo"
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        pure $ Two a b
instance (Eq a, Eq b)=> EqProp (Two a b) where
    (=-=) = eq

--  3.
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    Three f g h <*> Three a b c = Three (f `mappend` a) (g `mappend` b) (h c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        pure $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

--  4.
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)
instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    Three' f g h <*> Three' a b c = Three' (f `mappend` a) (g b) (h c)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        pure $ Three' a b c
instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

--  5.
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    Four f g h i <*> Four a b c d = Four (f `mappend` a) (g `mappend` b) (h `mappend` c) (i d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        pure $ Four a b c d
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

--  6.
data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)
instance Monoid a => Applicative (Four' a) where
    pure d = Four' mempty mempty mempty d
    Four' f g h i <*> Four' a b c d = Four' (f `mappend` a) (g `mappend` b) (h `mappend` c) (i d)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        pure $ Four' a b c d
instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

main :: IO ()
main = do
    putStrLn "\nApplicative Pair   ::  Pair ([Char], [Char], [Char])"
    quickBatch $ applicative (Pair ("a", "b", "c") ("d", "e", "f"))
    putStrLn "\nApplicative Pair   ::  Pair (Int, Int, Int)"
    quickBatch $ applicative (Pair ((5 :: Int), (6 :: Int), (7 :: Int)) ((5 :: Int), (6 :: Int), (7 :: Int)))
    putStrLn "\nApplicative Two  :: Two ([Char], [Char], [Char]) ([Char], [Char], [Char])"
    quickBatch $ applicative (Two ("a", "b", "c") ("d", "e", "f"))
    putStrLn "\nApplicative Three :: Three ([Char], [Char], [Char]) ([Char], [Char], [Char]) ([Char], [Char], [Char])"
    quickBatch $ applicative (Three ("a", "b", "c") ("d", "e", "f") ("g", "h", "i"))
    putStrLn "\nApplicative Three' :: Three' ([Char], [Char], [Char]) ([Char], [Char], [Char]) ([Char], [Char], [Char])"
    quickBatch $ applicative (Three' ("a", "b", "c") ("d", "e", "f") ("g", "h", "i"))
    putStrLn "\nApplicative Four :: Four ([Char], [Char], [Char]) ([Char], [Char], [Char]) ([Char], [Char], [Char]) ([Char], [Char], [Char])"
    quickBatch $ applicative (Four ("a", "b", "c") ("d", "e", "f") ("g", "h", "i") ("j", "k", "l"))
    putStrLn "\nApplicative Four' :: Four' ([Char], [Char], [Char]) ([Char], [Char], [Char]) ([Char], [Char], [Char]) ([Char], [Char], [Char])"
    quickBatch $ applicative (Four' ("a", "b", "c") ("d", "e", "f") ("g", "h", "i") ("j", "k", "l"))

