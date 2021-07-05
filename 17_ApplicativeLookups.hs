-- 17_ApplicativeLookups.hs
module ApplicativeLookups where
import Data.List (elemIndex)

--In the following exercises you will need to use the following
--terms to make the expressions typecheck:
--1. pure
--2. (<$>)
---- or fmap
--3. (<*>)

-- 1. added :: Maybe Integer
-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
-- *ApplicativeLookups> added
--Just 9

-- 2. y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
-- tupled = (,) y z
tupled = (,) <$> y <*> z
-- *ApplicativeLookups> tupled 
--Just (6,5)

-- 3. import Data.List (elemIndex)
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]
y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
-- maxed = max' x y
maxed = max' <$> x <*> y'
-- *ApplicativeLookups> maxed
--Just 3

--4.
xs = [1, 2, 3]
ys = [4, 5, 6]
x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys
y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys
summed :: Maybe Integer
--summed = sum $ (,) x y
summed = sum <$> ((,) <$> x'' <*> y'')
-- *ApplicativeLookups> summed 
--Just 5

-- Exercise: Identity Instance
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
    pure = Identity
    (<*>) (Identity a) (Identity b) = Identity (a b)
-- *ApplicativeLookups> fmap (*3) (Identity 1)
--Identity 3
-- *ApplicativeLookups> (Identity (*3)) <*> (Identity 3)
--Identity 9

--Exercise: Constant Instance
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)
instance Functor (Constant a) where
    fmap f (Constant a) = Constant a
instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant (a `mappend` b)
-- *ApplicativeLookups> fmap (*3) (Constant 6)
--Constant {getConstant = 6}
-- *ApplicativeLookups> (Constant (Data.Monoid.Sum 1)) <*> (Constant (Data.Monoid.Sum 2))
--Constant {getConstant = Sum {getSum = 3}}
-- *ApplicativeLookups> pure 7
--7

--Exercise: Fixer Upper
--Given the function and values provided, use (<$>) from Functor,
--(<*>) and pure from the Applicative typeclass to fill in missing
--bits of the broken code to make it work.
-- 1.
-- const <$> Just "Hello" <*> "World"
one = const <$> Just "Hello" <*> Just "World"
-- 2.
-- (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
-- *ApplicativeLookups> one
--Just "Hello"
-- *ApplicativeLookups> two
--Just (90,10,"Tierness",[1,2,3])

