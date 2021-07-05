module Main where
--module UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)
import Control.Monad (forM_)
import Data.Char (toUpper)

--instance Arbitrary Double

--1. -- for a function
half x = x / 2
-- this property should hold
halfIdentity = (*2) . half

propHalfIdentity :: Double -> Bool
propHalfIdentity x = halfIdentity x == x

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

propListOrdered :: [Int] -> Bool
propListOrdered x = (listOrdered $ sort x) == True

--3. Now we’ll test the associative and commutative properties
--of addition:
plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x
--Keep in mind these properties won’t hold for types based
--on IEEE-754 floating point numbers, such as Float or
--Double.

--4. Now do the same for multiplication
mltAssociative :: Integer -> Integer -> Integer -> Bool
mltAssociative x y z = x * (y * z) == (x * y) * z

mltCommutative :: Integer -> Integer -> Bool
mltCommutative x y = x * y == y * x

--5. We mentioned in one of the first chapters that there are
--some laws involving the relationship of quot and rem and
--div and mod. Write QuickCheck tests to prove them.
-- quot rem
--(quot x y)*y + (rem x y) == x
--(div x y)*y + (mod x y) == x

propQuotRem :: Integer -> Integer -> Bool
propQuotRem x y 
    | y == 0 = True
    | otherwise = (quot x y) * y + (rem x y) == x

propDivMod :: Integer -> Integer -> Bool
propDivMod x y
    | y == 0 = True
    | otherwise = (div x y) * y + (mod x y) == x

--6. Is (^) associative? Is it commutative? Use QuickCheck to see
--if the computer can contradict such an assertion.
-- answer: it is neither, nor, but this rule can be tested for power (z,y) > 0:
powerAssociative :: Integer -> Int -> Int -> Bool
powerAssociative x y z
    | y < 1 || z < 1 = True
    | otherwise = (x ^ y) ^ z == x ^ (y * z)
--7. Test that reversing a list twice is the same as the identity
--of the list:
propListTwiceReversed :: [Double] -> Bool
propListTwiceReversed x = (reverse . reverse) x == x
--8. Write a property for the definition of ($).
--f $ a = f a
--f . g = \x -> f (g x)
propDollar :: Double -> Bool
propDollar a = id $ a == id a

-- property with random Double function for the definition of ($)
propRandomDoublefunc :: Property
propRandomDoublefunc = forAll funcDoubleGen (\c x -> (c $ x) == (c x))

-- random True varies Int Generator
trueIntGen :: Gen Int
trueIntGen = coarbitrary True arbitrary

-- Int Generator
intGen :: Gen Int
intGen = arbitrary

-- random False varies Int Generator
-- to show:   Prelude> sample falseIntGen
falseIntGen :: Gen Int
falseIntGen = coarbitrary False arbitrary

-- random True varies Bool Generator
trueBoolGen :: Gen Bool
trueBoolGen = coarbitrary True arbitrary

-- random Bool Generator
boolGen :: Gen Bool
boolGen = arbitrary

-- random Int Function
funcIntGen :: Gen (Int -> Int)
funcIntGen = arbitrary

-- random Double function
funcDoubleGen :: Gen (Double -> Double)
funcDoubleGen = arbitrary

sfunc :: (Integer -> Integer)
sfunc = \x -> (+ 8) x

-- property with random Int function for the proof of existance of an computable input value
propRandomIntfunc :: Property
propRandomIntfunc = forAll funcIntGen (\c x -> c x == makeNormalfunc c x)

makeNormalfunc func x = (+ (func x - x)) x

-- random Bool function
funcBoolGen :: Gen (Bool -> Bool)
funcBoolGen = arbitrary

instance Show (a -> b) where
    show _ = "<function>"

--intGen :: Gen (Int -> Int)
--intGen = coarbitrary funcIntGen arbitrary

runGenFn :: (Arbitrary a, Arbitrary b, Show a, Show b) => Gen (a -> b) -> [a] -> IO ()
runGenFn g as = do
    fns  <- sample' g
    forM_ fns $ \f -> do
        forM_ as $ \a -> putStrLn $ show a ++ " => "
                                           ++ show (f a)
        putStrLn ""

-- show random Int functions:
-- *UsingQuickCheck> runGenFn funcIntGen [1,2,3]
--1 => 3
--2 => 9
--3 => 11

--1 => -2
--2 => -3
--3 => 9
-- ...
 
 -- show random Double functions:
 -- *UsingQuickCheck> runGenFn funcDoubleGen [1,2,3]
--1.0 => 11.747238226458489
--2.0 => 11.693685217745461
--3.0 => 1.505725619895114

--1.0 => 192.91369425052525
--2.0 => 6.025585029349824
--3.0 => 55.67552627610489
-- ...

-- already defined in `Test.QuickCheck.Arbitrary´
--instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
--  arbitrary = promote (\a -> coarbitrary a arbitrary)

--promote :: (a -> Gen b) -> (Gen (a->b))
--promote f = Gen $ \r ->
--    \a -> let Gen h = f a
--            in h r
--newtype Gen a = Gen
--        { unGen :: StdGen -> a }

--9. See if these two functions are equal:
--foldr (:) == (++)
propStringAdd :: String -> String -> Bool
propStringAdd x y = foldr (:) y x == (++) x y
--foldr (++) [] == concat
propConcat :: [String] -> Bool
propConcat x = foldr (++) [] x == concat x
--10. Hm. Is that so?
--f n xs = length (take n xs) == n
-- -->>>  it is, but only for positive n AND n <= length xs
f :: Int -> [Char] -> Bool
f n xs 
    | n < 0 = True
    | n > length xs = True
    | otherwise = length (take n xs) == n
--11. Finally, this is a fun one. You may remember we had you
--compose read and show one time to complete a “round
--trip.” Well, now you can test that it works:
--rs x = (read (show x)) == x
rs :: [Char] -> Bool
rs x = (read (show x)) == x

-- failing because of limited precision
square y = y * y
squareIdentity :: Double -> Bool
squareIdentity x = (square . sqrt) x == x

twice f = f . f
fourTimes = twice . twice
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs
idempotence :: [Char] -> Bool
idempotence x = (capitalizeWord x
    == twice capitalizeWord x)
    &&
    (capitalizeWord x
    == fourTimes capitalizeWord x)
idempotence' :: [Char] -> Bool
idempotence' x =
    (sort x
    == twice sort x)
    &&
    (sort x
    == fourTimes sort x)

-- Make a Gen random generator for the datatype
-- 1. Equal probabilities for each.
data Fool = Fulse | Frue deriving (Eq, Show)
instance Arbitrary Fool where
  arbitrary = elements [Fulse,Frue]
foolGen :: Gen Fool
foolGen = arbitrary
-- *UsingQuickCheck > sample foolGen
-- Fulse
-- Frue
-- ...

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue
data Fool' = Fulse' | Frue' deriving (Eq, Show)
instance Arbitrary Fool' where
  arbitrary = frequency [(2, return Fulse'),(1, return Frue')]
foolGen' :: Gen Fool'
foolGen' = arbitrary
-- *UsingQuickCheck > sample foolGen
-- Fulse
-- Fulse
-- Frue
-- ...

main :: IO ()
main = do
    putStrLn "testing halfIdentity   ((*2) . (x/2)) x == x   ... "
    quickCheck propHalfIdentity
--    verboseCheck propHalfIdentity      -- for detailed output of test data
    putStrLn "\ntesting listOrdered ... "
    quickCheck propListOrdered
    putStrLn "\ntesting plusAssociative    x + (y + z) == (x + y) + z   ... "
    quickCheck plusAssociative
    putStrLn "\ntesting plusCommutative    x + y == y + x   ... "
    quickCheck plusCommutative
    putStrLn "\ntesting mltAssociative   x * (y * z) == (x * y) * z   ..."
    quickCheck mltAssociative
    putStrLn "\ntesting mltCommutative   x * y == y * x   ... "
    quickCheck mltCommutative
    putStrLn "\ntesting propQuotRem   (quot x y) * y + (rem x y) == x   (y /= 0)   ... "
    quickCheck propQuotRem
    putStrLn "\ntesting propDivMod   (div x y) * y + (mod x y) == x   (y /= 0)   ... "
    quickCheck propDivMod
    putStrLn "\ntesting powerAssociative   (x ^ y) ^ z == x ^ (y * z)   (y >= 1 && z >= 1)  ... "
    quickCheck powerAssociative
    putStrLn "\ntesting propListTwiceReversed   (reverse . reverse) x == x   ... "
    quickCheck propListTwiceReversed
    putStrLn "\ntesting propDollar   id $ a == id a   ... "
    quickCheck propDollar
--    verboseCheck propDollar
    putStrLn "\ntesting propRandomDoublefunc   forAll funcDoubleGen (\\c x -> (c $ x) == (c x))   ..."
    quickCheck propRandomDoublefunc
--    putStrLn "\ntesting verbosed propRandomDoublefunc   forAll funcDoubleGen (\c x -> (c $ x) == (c x))   ..."
--    verboseCheck propRandomDoublefunc
    putStrLn "\ntesting propStringAdd   foldr (:) y x == (++) x y   ..."
    quickCheck propStringAdd
    putStrLn "\ntesting propConcat   foldr (++) [] x == concat x   ..."
    quickCheck propConcat
    putStrLn "\ntesting f   length (take n xs) == n   (n >= 0 && n <= length xs)   ..."
    quickCheck f
    putStrLn "\ntesting rs   (read (show x)) == x   ..."
    quickCheck rs
    putStrLn "\ntesting squareIdentity   (square . sqrt) x == x   (fails because of limited precision)   ..."
    quickCheck squareIdentity
    putStrLn "\ntesting idempotence   (capitalizeWord x == (capitalizeWord . capitalizeWord) x)   ..."
    quickCheck idempotence
    putStrLn "\ntesting idempotence'   (sort x == (sort . sort) x)   ..."
    quickCheck idempotence'
    putStrLn "\ntesting propRandomIntfunc   \\c x -> c x == makeNormalfunc c x   ..."
    quickCheck propRandomIntfunc

