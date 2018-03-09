-- 16_11_Exercise_Possibly.hs
module ExercisePossibly where

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

--Prelude> liftedInc (Just 1)
--Just 2
--Prelude> liftedInc Nothing
--Nothing
--Prelude> liftedShow (Just 1)
--Just "1"
--Prelude> liftedShow Nothing
--Nothing

--     Exercise: Possibly (Maybe)
--Write a Functor instance for a datatype identical to Maybe. We’ll
--use our own datatype because Maybe already has a Functor in-
--stance and we cannot make a duplicate one.
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)
-- If it helps, you’re basically writing the following function:
--applyIfJust :: (a -> b) -> Maybe a -> Maybe b

-- *ExercisePossibly> (*3) <$> LolNope 
--LolNope
-- *ExercisePossibly> (*3) <$> Yeppers 4
--Yeppers 12

--     Sum (Either)
--Write a Functor instance for a datatype identical to Either.
--We’ll use our own datatype because Either has a Functor
--instance.
data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

--Your hint for this one is that you’re writing the following
--function.
--applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b

-- *ExercisePossibly> fmap (*3) $ First 5
--First 5
-- *ExercisePossibly> fmap (*3) $ Second 5
--Second 15

-- 2.
--Why is a Functor instance that applies the function only to
--First, Either’s Left, impossible? We covered this earlier.
-- Answer: fmap can only be applied to one argument 
--        of a sum type, which must be reduced to * -> *

