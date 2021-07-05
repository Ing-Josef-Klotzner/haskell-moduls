{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Data.Semigroup
import Debug.Trace

newtype Combine a b = Combine { unCombine :: a -> b }

newtype PlusInt = PlusInt Int deriving (Show, Arbitrary, Eq)

instance Semigroup PlusInt where
  PlusInt a <> PlusInt b = PlusInt $ a + b

instance Semigroup b => Semigroup (Combine a b) where
  Combine f1 <> Combine f2 = Combine (\a -> f1 a <> f2 a)


instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

main :: IO ()
main = verboseCheck $ property testMonoid
  where
    testMonoid :: Gen Bool
    testMonoid = do
      f1 :: (Combine Int PlusInt) <- arbitrary
      f2 <- arbitrary
      f3 <- arbitrary
      x :: Int  <- traceShowId <$> arbitrary
      pure $ traceShowId (unCombine (f1 <> f2 <> f3) x)  == unCombine (f1 <> (f2 <> f3)) x
