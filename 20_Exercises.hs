-- 20_Exercises.hs
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Exercises20 where
import Data.Foldable (foldMap)
import Data.Monoid (getSum, Sum(..), getProduct, Product(..))
import Data.Coerce (Coercible, coerce)
import Data.Maybe (fromMaybe)

--Exercises: Library Functions
--Implement the functions in terms of foldMap or foldr from
--Foldable, then try them out with multiple types that have
--Foldable instances.
--1. This and the next one are nicer with foldMap, but foldr is
--fine too.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum #. foldMap Sum
-- *Exercises20> sum' [1,2,3,4,5,6]
--21

--2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct #. foldMap Product
-- *Exercises20> product' [1,2,3,4,5,6]
--720

--3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' = any . (==)
-- *Exercises20> elem' 17 [15,16,17,18]
--True
--4.
newtype Min a = Min {getMin :: Maybe a} deriving Show
minimum' :: (Foldable t, Monoid (Min a)) => t a -> Maybe a
minimum' x = getMin $ foldMap (Min . Just) x
instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mappend (Min Nothing) m = m
    mappend m (Min Nothing) = m
    mappend (Min a) (Min a') = Min $ min a a'
-- *Exercises20> minimum' [3,8,4,9,1]
--Just 1
--5.
newtype Max a = Max {getMax :: Maybe a}
maximum' :: (Foldable t, Monoid (Max a)) => t a -> Maybe a
maximum' x = getMax $ foldMap (Max . Just) x
instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mappend (Max Nothing) m = m
    mappend m (Max Nothing) = m
    mappend (Max a) (Max a') = Max $ max a a'
-- *Exercises20> maximum' [3,8,4,9,1]
--Just 9
--6.
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

null'' :: (Foldable t) => t a -> Bool
null'' y = (\_ -> (length' y == 0)) y

null''' :: (Foldable t) => t a -> Bool
null''' z = (\w -> w == 0) $ getSum $ foldMap (\_ -> Sum 1) z

-- *Exercises20> null' []
--True
-- *Exercises20> null' Nothing
--True
-- *Exercises20> null' [1]
--False
--7.
length' :: (Foldable t) => t a -> Int
length' x = getSum $ foldMap (\_ -> Sum 1) x

length'' :: (Foldable t) => t a -> Int
length'' = foldr (\_ n -> n + 1) 0
-- *Exercises20> length' [3,8,4,9,1]
--5

--8. Some say this is all Foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' y = foldMap (\x -> x : []) y

toList'' :: (Foldable t) => t a -> [a]
toList'' y = foldr (\x xs -> x : xs) [] y
-- *Exercises20> toList' (Just 1)
--[1]
-- *Exercises20> toList' (Right 1)
--[1]
-- *Exercises20> toList' Nothing
--[]

--9. Hint: use foldMap.
---- | Combine the elements
------ of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' y = foldMap (mappend mempty) y
fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' y = foldr mappend mempty y
-- *Exercises20> fold' [Sum 1,Sum 2,Sum 3]
--Sum {getSum = 6}

--10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> (f x) `mappend` y) mempty
-- *Exercises20> foldMap' (mappend mempty) [Sum 1,Sum 2,Sum 3]
--Sum {getSum = 6}



--20.6
--Chapter Exercises
--Write Foldable instances for the following datatypes.

-- 1.
data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap f (Constant a) = f a
-- *Exercises20> foldMap (\x -> x : []) (Constant 5)
--[5]
--2.
data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f (Two a b) = f b
-- *Exercises20> foldMap (\x -> x : []) (Constant 5)
--[5]
--3.
data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c
-- *Exercises20> foldMap (\x -> x : []) (Three 5 6 7)
--[7]
--4.
data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap f (Three' a b b') = f b `mappend` f b'
-- *Exercises20> foldMap (\x -> x : []) (Three' 5 6 7)
--[6,7] 
--5.
data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap f (Four' a b b' b'') = f b `mappend` f b' `mappend` f b''
-- *Exercises20> foldMap (\x -> x : []) (Four' 4 5 6 7)
--[5,6,7]

--Thinking cap time. Write a filter function for Foldable types
--using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF fil y = foldMap (\x -> if fil x then pure x else mempty) y
-- *Exercises20> (filterF even [0,1,2,3]) :: [Int]
--[0,2]
