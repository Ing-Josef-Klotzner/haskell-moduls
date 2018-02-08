-- 9_12_Exercises.hs
module Exercises_9_12 where
import Data.Char
import Data.List

-- write a function that filters all the uppercase letters out of a String
filterUpper :: [Char] -> [Char]
filterUpper x = filter isUpper x

--Write a function that will capitalize the first letter of a
--string and return the entire string. For example, if given
--the argument “julie,” it will return “Julie.”
capital :: [Char] -> [Char]
capital "" = ""
capital x = [toUpper (head x)] ++ tail x

--Now make a new version of that function that is recursive
--such that if you give it the input “woot” it will holler back
--at you “WOOT.” The type signature won’t change, but
--you will want to add a base case.
capitalize :: [Char] -> [Char]
capitalize "" = ""
capitalize x = [toUpper (head x)] ++ capitalize (tail x)

--To do the final exercise in this section, we’ll need another
--standard function for lists called head. Query the type of
--head and experiment with it to see what it does. Now write
--a function that will capitalize the first letter of a String
--and return only that letter as the result.
capitalFirst :: [Char] -> [Char]
capitalFirst "" = ""
capitalFirst x = [toUpper (head x)]

--Cool. Good work. Now rewrite it as a composed function.
capitalFirst' :: [Char] -> [Char]
capitalFirst' "" = ""
capitalFirst' x = [toUpper $ head x]

--Then, for fun, rewrite it pointfree
capitalFirst'' :: [Char] -> [Char]
--capitalFirst'' "" = ""     -- problem on empty string with this version!
capitalFirst'' =  (\x -> [x]) . toUpper . head

-- caesar right shift lower case (a-z)
cs_rt_lc :: Int -> Char -> Char
cs_rt_lc s x = chr ((rem (ord x + s - ord 'a') 26) + ord 'a')

-- caesar right shift upper case (A-Z)
cs_rt_uc :: Int -> Char -> Char
cs_rt_uc s x = chr ((rem (ord x + s - ord 'A') 26) + ord 'A')

-- caesar left shift lower case (a-z)
cs_lt_lc :: Int -> Char -> Char
cs_lt_lc s x = chr ((rem (ord x - s - ord 'z') 26) + ord 'z')

-- caesar left shift upper case (A-Z)
cs_lt_uc :: Int -> Char -> Char
cs_lt_uc s x = chr ((rem (ord x - s - ord 'Z') 26) + ord 'Z')

-- for uncaesar just negate s
caesar :: Int -> Char -> Char
caesar s x
    | s > 0 && isLower x = cs_rt_lc s x
    | s > 0 && isUpper x = cs_rt_uc s x
    | s < 0 && isLower x = cs_lt_lc (-s) x
    | s < 0 && isUpper x = cs_lt_uc (-s) x
    | otherwise = x

-- for those not understanding last comment:
uncaesar :: Int -> Char -> Char
uncaesar s x = caesar (-s) x


myAnd'' :: [Bool] -> Bool
myAnd'' [] = True
myAnd'' (x:xs) =
    if x == False
    then False
    else myAnd'' xs
-- direct recursion, using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs
-- And now the fun begins:
-- myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
-- myAny returns True if a -> Bool applied to any of the values
-- in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs


-- After you write the recursive myElem, write another version
-- that uses any. The built-in version of elem in GHC 7.10 and
-- newer has a type that uses Foldable instead of the list type
-- specifically. You can ignore that and write the concrete
-- version that works only for list.
myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = x == e || myElem e xs
-- Prelude> myElem 1 [1..10]
-- True
-- Prelude> myElem 1 [2..10]
-- False
myElem'' :: Eq a => a -> [a] -> Bool
--myElem'' e [] = False
myElem'' e l = any (e ==) l


-- Implement myReverse.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
-- Prelude> myReverse "blah"
-- "halb"
-- Prelude> myReverse [1..5]
-- [5,4,3,2,1]

-- squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs


-- squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs
-- Prelude> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
-- Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
-- "WO 1 HOO WO 2 HOO WO 3 HOO "


--squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- or 
-- squishAgain l = squishMap (concat) [l]



-- myMaximumBy takes a comparison function and a list and
-- returns the greatest element of the list based on the last
-- value that the comparison returned GT for. If you import
-- maximumBy from Data.List, you’ll see the type is:
-- (a -> a -> Ordering) -> [a] -> a

myMaximumBy''' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy''' cmp = foldr1 myMax
  where
    myMax a b = case cmp a b of
                  GT -> a
                  _ -> b

myMaximumBy'' :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy'' f (x:xs) = go f x (head xs) (x:xs)
    where go fun x a (y:ys)
            | xs == [] = x
            | ys == [] && fun x a == LT && fun a y == GT = a
            | ys == [] && fun x a == LT && fun a y == LT = y
            | ys == [] && fun x a == GT && fun x y == GT = x
            | ys == [] && fun x a == GT && fun x y == LT = y
            | fun x a == LT = go fun a y ys
            | otherwise = go fun x y ys

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy cmp = foldr myMax Nothing
  where
    myMax a Nothing = Just a
    myMax a (Just b) = 
      case cmp a b of
        GT -> return a
        _  -> return b

-- is it possible to solve shorter? (Roberts Lösung)
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' cmp = foldr1 (\a b -> if cmp a b == GT then a else b)

-- yes! even shorter!
myMaximumBy'''' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy'''' cmp = head . (sortBy . flip) cmp

myMaximumBy :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f x (head xs) (x:xs)
    where go fun x a lst
            | xs == [] = x
            | lst == [] && fun x a == LT = a
            | lst == [] && fun x a == GT = x
            | lst == [] = x
            | fun x a == LT = go fun a (head lst) (tail lst)
            | otherwise = go fun x (head lst) (tail lst)

-- *Exercises_9_12> myMaximumBy compare [54,755,4,5,2999,444,3000]
-- 3000
 
--myMinimumBy takes a comparison function and a list and
--returns the least element of the list based on the last value
--that the comparison returned LT for.
myMinimumBy :: Eq a => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f x (head xs) (x:xs)
    where go fun x a lst
            | xs == [] = x
            | lst == [] && fun x a == LT = x
            | lst == [] && fun x a == GT = a
            | fun x a == GT = go fun a (head lst) (tail lst)
            | otherwise = go fun x (head lst) (tail lst)

-- *Exercises_9_12> myMinimumBy compare [54,755,4,5,1,4444,3]
-- 1

-- myMinimumBy is possible even shorter:
myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' cmp = head . sortBy cmp

myMaximum :: (Ord a) => [a] -> a
myMaximum  = head . (sortBy . flip) compare
 
myMinimum :: (Ord a) => [a] -> a
myMinimum = head . sortBy compare

