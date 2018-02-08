-- 6_EqInstances.hs
module EqInstances where

data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
--    (TisAn a) == (TisAn b) = a == b -- general for all integer True when equal
    TisAn 1 == TisAn 1 = True
    TisAn _ == TisAn _ = False

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    Two 0 1 == Two 0 1 = True
    Two _ _ == Two _ _ = False

data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    TisAnInt 0 == TisAnInt 0 = False
    TisAString "servus" == TisAString "servus" = True
    TisAString "servus" == TisAnInt 1 = True
    TisAnInt _ == TisAnInt _ = True
    TisAString _ == TisAString _ = False
    TisAString _ == TisAnInt _ = False
    TisAnInt _ == TisAString _ = False

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple c d) = a == c && b == d

data Which a =
    ThisOne a
    | ThatOne a

instance Eq a => Eq (Which a) where
    ThisOne a == ThatOne b = a == b
    ThisOne a == ThisOne b = a == b
    ThatOne a == ThatOne b = a == b
    ThatOne a == ThisOne b = a == b

data EitherOr a b =
    Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello b) = a == b
    (==) (Goodbye c) (Goodbye d) = c == d
