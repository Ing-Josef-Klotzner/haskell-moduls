-- 10_10_Exercises.hs
module Exercises_10_10 where

-- 1. Given the following sets of consonants and vowels:CHAPTER 10. DATA STRUCTURE ORIGAMI
stops = "pbtdkg"
vowels = "aeiou"
-- a) Write a function that takes inputs from stops and
--    vowels and makes 3-tuples of all possible stop-vowel-
--    stop combinations. These will not all correspond to
--    real words in English, although the stop-vowel-stop
--    pattern is common enough that many of them will.
wearedWords :: [(Char, Char, Char)]
wearedWords = [(x,y,z) | x <- stops, y <- vowels, z <- stops]


-- b) Modify that function so that it only returns the com-
--    binations that begin with a p.
wearedPWords :: [(Char, Char, Char)]
wearedPWords = [(x,y,z) | x <- "p", y <- vowels, z <- stops]

-- c) Now set up lists of nouns and verbs (instead of stops
--    and vowels) and modify the function to make tuples
--    representing possible noun-verb-noun sentences
nouns = ["cat", "table", "dog", "house", "mouse", "garden"]
verbs = ["rock", "do", "is", "use", "take","make"]
wearedPhrases :: [(String, String, String)]
wearedPhrases = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. What does the following mystery function do? What is
--    its type? Try to get a good sense of what it does before
--    you test it in the REPL to verify it.
seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-- answer: this function returns an integer of the average characters per word of a sentence


-- 3. We’d really like the answer to be more precise. Can you
--    rewrite that using fractional division?
-- seeAvgCharOfWords :: String -> Double
seeAvgCharOfWords x =
        fromIntegral (sum (map length (words x)))
        / fromIntegral (length (words x))


--Rewriting functions using folds
--    In the previous chapter, you wrote these functions using direct
--    recursion over lists. The goal now is to rewrite them using
--    folds. Where possible, to gain a deeper understanding of
--    folding, try rewriting the fold version so that it is point-free.
--    Point-free versions of these functions written with a fold
--    should look like:
--    myFunc = foldr f z
--    So for example with the and function:
--    -- Again, this type will be less
--    -- reusable than the one in GHC 7.10
--    -- and newer. Don't worry.
--    -- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == False
    then False
    else myAnd xs

myAndF :: [Bool] -> Bool
myAndF = foldr (\x y -> if x == False then False else y) True

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd' xs

myAndF' :: [Bool] -> Bool
myAndF' = foldr (&&) True

-- 1. myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to any of the values
-- in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny fB = or . map fB
--    Example for validating myAny:
--    Prelude> myAny even [1, 3, 5]
--    False
--    Prelude> myAny odd [1, 3, 5]
--    True

--3. Write two versions of myElem. One version should use
--   folding and the other should use any.
myElem :: Eq a => a -> [a] -> Bool
myElem x = or . map (== x)

-- using any
myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = any (x ==)

-- using foldr
myElemF :: Eq a => a -> [a] -> Bool
myElemF e = foldr (\x y -> if x == e then True else y) False
--    Prelude> myElem 1 [1..10]
--    True
--    Prelude> myElem 1 [2..10]
--    False

--4. Implement myReverse, don’t worry about trying to make
--   it lazy.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
--    Prelude> myReverse "blah"
--    "halb"
--    Prelude> myReverse [1..5]
--    [5,4,3,2,1]

--5. Write myMap in terms of foldr. It should have the same
--    behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

--6. Write myFilter in terms of foldr. It should have the same
--    behavior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fil = foldr (\x y -> if fil x then x:y else y) []

--7. squish flattens a list of lists into a list
squish' :: [[a]] -> [a]
squish' = foldr (++) []

--8. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
--Prelude> squishMap (\x -> [1, x, 3]) [2]
--[1,2,3]
--Prelude> let f x = "WO " ++ [x] ++ " OT "
--Prelude> squishMap f "blah"
--"WO b OT WO l OT WO a OT WO h OT "

--9. squishAgain flattens a list of lists into a list. This time re-use
--    the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--10. myMaximumBy takes a comparison function and a list and
--    returns the greatest element of the list based on the last
--    value that the comparison returned GT for.
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' cp (x:xs) = foldr (\x y -> if cp x y == GT then x else y) x xs
-- or with foldr1 pointfree:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cp = foldr1 (\x y -> if cp x y == GT then x else y)
--Prelude> myMaximumBy (\_ _ -> GT) [1..10]
--1
--Prelude> myMaximumBy (\_ _ -> LT) [1..10]
--10
--Prelude> myMaximumBy compare [1..10]
--10
--11. myMinimumBy takes a comparison function and a list and
--    returns the least element of the list based on the last value
--    that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cp (x:xs) = foldr (\x y -> if cp x y == LT then x else y) x xs
--    Prelude> myMinimumBy (\_ _ -> GT) [1..10]
--    10
--    Prelude> myMinimumBy (\_ _ -> LT) [1..10]
--    1
--    Prelude> myMinimumBy compare [1..10]
--    1
