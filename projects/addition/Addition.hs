module Main where
import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is\
            \ 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "10000000 divided by 1 is\
            \ 10000000 remainder 0 (speed test using faster version dividedBy)" $ do
            dividedBy 10000000 1 `shouldBe` (10000000, 0)
--        it "1000000 divided by 1 is\
--            \ 1000000 remainder 0 (speed test using dividedBy''' - original version)" $ do
--            dividedBy''' 1000000 1 `shouldBe` (1000000, 0)
        it "10000000000000000000000000000 divided by 3 is\
            \ 10000000 remainder 0 (speed test using dividedBy)" $ do
            dividedBy 10000000000000000000000000000 3 `shouldBe` (3333333333333333333333333333, 1)
        it "10000000000000000000000000000 divided by 3852216622489216728921 is\n\
            \  2595908 remainder 51947262379660144732 (using dividedBy)" $ do
            dividedBy 10000000000000000000000000000 3852216622489216728921 `shouldBe` (2595908,51947262379660144732)

dividedBy''' :: Integral a => a -> a -> (a, a)
dividedBy''' num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

dividedBy :: (Integral b, Read b, Show b) => b -> b -> (b, b)
dividedBy x y
    | y == 0 = (0,0)
    | y < 0 && x < 0 = (fst $ dividedBy'' (abs x) (abs y), -(snd $ dividedBy'' (abs x) (abs y)))
    | y < 0 && x > 0 = (-(fst $ dividedBy'' x (abs y)), snd $ dividedBy'' x (abs y))
    | y > 0 && x < 0 = (-(fst $ dividedBy'' (abs x) y), -(snd $ dividedBy'' (abs x) y))
    | otherwise = dividedBy'' x y
    

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)

-- 8765 / 234 = 3 7      dividedBy' 8765 2340 =  3 1745 Rest
-- 1745                  dividedBy' 1745 234 = 7 107 Rest
--  107            ... Result (37, 107)
-- version using above method for calculation
dividedBy'' :: (Integral a, Read a, Show a) => a -> a -> (a, a)
dividedBy'' x y = go x y (divResultPot x y) 0
    where go a b pot res
            | pot < 0 = (res, a)
            | otherwise = go (snd $ dividedBy' a (mulIntx10_n b pot)) (b) (pot - 1)
                            (res + (mulIntx10_n (fst $ dividedBy' a (mulIntx10_n b pot)) pot ))

-- find power of highest digit of result
divResultPot :: (Integral a, Num a1, Read a, Show a) => a -> a -> a1
divResultPot x y = go x y 0
    where go a b dgt
            | a < b = dgt - 1
            | otherwise = go a (mulIntx10_n b 1) (dgt + 1)

-- multiply Integer x by 10^n
mulIntx10_n :: (Read a, Show a, Integral a) => a -> a -> a
mulIntx10_n x n = read (show x ++ nTimesZero n)  --read (intToString x ++ nTimesZero n)

nTimesZero :: (Eq a, Num a) => a -> [Char]
nTimesZero n = go n ""
    where go ct str
            | ct == 0 = str
            | otherwise = go (ct - 1) (str ++ "0")

