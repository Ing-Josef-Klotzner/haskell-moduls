--16_17_Chapter_Exercises.hs
{-# LANGUAGE FlexibleInstances #-}
module Exercises_16_17 where
import GHC.Arr

--Determine if a valid Functor can be written for the datatype
--provided.
-- 1.
data Bool = False | True 
-- Answer: Functor instances can't be written for type constants

-- 2.
data BoolAndSomethingElse a = False' a | True' a
instance Functor (BoolAndSomethingElse) where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)
instance Show a => Show (BoolAndSomethingElse a) where
    show (True' a) = "True' " ++ show a
    show (False' a) = "False' " ++ show a
-- *Exercises_16_17> fmap (*3) $ False' 445
--False' 1335
-- *Exercises_16_17> fmap (*3) $ True' 25
--True' 75

-- 3.
data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor (BoolAndMaybeSomethingElse) where
    fmap f Falsish = Falsish
    fmap f (Truish a) = Truish (f a)
instance Show a => Show (BoolAndMaybeSomethingElse a) where
    show Falsish = "Falsish"
    show (Truish a) = "Truish " ++ show a

-- 4.
--Use the kinds to guide you on this one, don’t get too hung
--up on the details.
newtype Mu f = InF { outF :: f (Mu f) }
-- following has kind (* -> *) -> *   - not applicable to functor
--instance Functor Mu where
-- following has kind *   - not applicable to functor
--instance Functor f => Functor (Mu f) where
--    fmap = undefined

-- 5.
-- Again, follow the kinds and ignore the unfamiliar parts
data D = D (Array Word Word) Int Int
-- not possible to create instance Functor for D as it's kind is *

--Rearrange the arguments to the type constructor of the
--datatype so the Functor instance works.
-- 1.
data Sum a b = First b | Second a
instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b
instance (Show a, Show b) => Show (Sum a b) where
    show (First a) = "First " ++ show a
    show (Second a) = "Second " ++ show a
-- *Exercises_16_17> fmap (*33) (Second 45)
--Second 45
-- *Exercises_16_17> fmap (*33) (First 45)
--First 1485

-- 2.
data Company a b c = DeepBlue a b | Something c deriving Show
instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap f (DeepBlue a c) = DeepBlue a c
-- *Exercises_16_17> fmap (*33) (DeepBlue "super" 45)
--DeepBlue "super" 45
-- *Exercises_16_17> fmap (*33) (Something 45)
--Something 1485

-- 3.
data More a b = L b a b  | R a b a deriving (Eq, Show)
instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'
--Keeping in mind that it should result in a Functor that does
--the following:
--Prelude> fmap (+1) (L 1 2 3)
--L 2 2 4
--Prelude> fmap (+1) (R 1 2 3)
--R 1 3 3

-- *Exercises_16_17> fmap (*33) (L 23 "hi" 3)
--L 759 "hi" 99
-- *Exercises_16_17> fmap (*33) (R "super" 45 "hi")
--R "super" 1485 "hi"

--Write Functor instances for the following datatypes.
-- 1.
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2.
--No, it’s not interesting by itself.
data K' a b = K' a
instance Functor (K' a) where
  fmap _ (K' a) = K' a
-- 3. {-# LANGUAGE FlexibleInstances #-}
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K a b = K a
instance Show a => Show (K a b) where
    show (K a) = "K " ++ show a
-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip $ K (f b)
-- *Exercises_16_17> fmap (*33) (Flip (K 22))
--Flip K 726

--4.
data EvilGoateeConst a b = GoatyConst b
--No, it doesn’t do anything interesting. No magic here or
--in the previous exercise. If it works, you succeeded.
instance Functor (EvilGoateeConst b) where
    fmap f (GoatyConst b) = GoatyConst (f b) 
instance Show b => Show (EvilGoateeConst a b) where
    show (GoatyConst b) = "GoatyConst " ++ show b
-- *Exercises_16_17> fmap (*3) (GoatyConst 4)
--GoatyConst 12

--5.
-- Do you need something  extra to make the instance work?
data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f' (LiftItOut f) = LiftItOut (fmap f' f)
instance Functor f => Show (LiftItOut f a) where
    show (LiftItOut f) = "LiftItOut <function>"

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

--7.
--Don’t ask for more typeclass instances than you need. You
--can let GHC tell you what to do.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

--8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

-- 9. You’ll need to use recursion.
data List a = Nil | Cons a (List a)

--10. A tree of goats forms a Goat-Lord, fearsome poly-creature.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
-- A VERITABLE HYDRA OF GOATS

--11. You’ll use an extra functor for this one, although your so-
--lution might do it monomorphically without using fmap.
--Keep in mind that you will probably not be able to vali-
--date this one in the usual manner. Do your best to make
--it work.
data TalkToMe a = Halt | Print String a | Read (String -> a)

