--17_9_Chapter_Exercises.hs
{-# LANGUAGE FlexibleContexts #-}
module ChapterExercises17 where
import Control.Applicative (liftA3)

--17.9
--Chapter Exercises
--Given a type that has an instance of Applicative, specialize the
--types of the methods. Test your specialization in the REPL.
--One way to do this is to bind aliases of the typeclass methods
--to more concrete types that have the type we told you to fill
--in.
--  1. -- Type
--  []
-- Methods
pureList :: a -> [a]
pureList = pure

appList :: [(a -> b)] -> [a] -> [b]
appList = (<*>)

-- *ChapterExercises17> pureList "hi"
--["hi"]
-- *ChapterExercises17> appList [(*3),(+5)] [1,5,9]
--[3,15,27,6,10,14]

--  2. -- Type
--  IO
-- Methods
pureIO :: a -> IO a
pureIO = pure
appIO :: IO (a -> b) -> IO a -> IO b
appIO = (<*>)

-- *ChapterExercises17> pureIO "text"
--"text"
-- *ChapterExercises17> appIO (pureIO (++ " added!")) getLine
--line
--"line added!"

--  3. -- Type
--  (,) a
-- Methods
pureTuple :: Monoid a => b -> (a, b)
pureTuple = pure
appTuple :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)
appTuple = (<*>)

-- *ChapterExercises17> pureTuple 4
--((),4)
-- *ChapterExercises17> appTuple ("test",(*8)) ("tuple",7)
--("testtuple",56)

--  4. -- Type
--  (->) e
-- Methods
pureFun :: b -> (a -> b)
pureFun = pure
appFun :: (a -> b -> c) -> (a -> b) -> (a -> c)
appFun = (<*>)
-- *ChapterExercises17> pureFun 4 $ 5
--4
-- *ChapterExercises17> f x y = x * y
-- *ChapterExercises17> appFun f (*7) 5
--175

--instances of datatypes in separate module
--17_9_Ch_Exs_Applicative_Instances.hs

--Combinations
--Remember the vowels and stops exercise in the folds chapter?
--Write the function to generate the possible combinations of
--three input lists using liftA3 from Control.Applicative.
--import Control.Applicative (liftA3)
stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- *ChapterExercises17> combos stops vowels stops
--[('p','a','p'),('p','a','b'),('p','a','t'),('p','a','d'),('p','a','k'),('p','a','g'),('p','e','p'),('p','e','b'),('p','e','t'),('p','e','d'),('p','e','k'),('p','e','g'),('p','i','p'),('p','i','b'),('p','i','t'),('p','i','d'),('p','i','k'),('p','i','g'),('p','o','p'),('p','o','b'),('p','o','t'),('p','o','d'),('p','o','k'),('p','o','g'),('p','u','p'),('p','u','b'),('p','u','t'),('p','u','d'),('p','u','k'),('p','u','g'),('b','a','p'),('b','a','b'),('b','a','t'),('b','a','d'),('b','a','k'),('b','a','g'),('b','e','p'),('b','e','b'),('b','e','t'),('b','e','d'),('b','e','k'),('b','e','g'),('b','i','p'),('b','i','b'),('b','i','t'),('b','i','d'),('b','i','k'),('b','i','g'),('b','o','p'),('b','o','b'),('b','o','t'),('b','o','d'),('b','o','k'),('b','o','g'),('b','u','p'),('b','u','b'),('b','u','t'),('b','u','d'),('b','u','k'),('b','u','g'),('t','a','p'),('t','a','b'),('t','a','t'),('t','a','d'),('t','a','k'),('t','a','g'),('t','e','p'),('t','e','b'),('t','e','t'),('t','e','d'),('t','e','k'),('t','e','g'),('t','i','p'),('t','i','b'),('t','i','t'),('t','i','d'),('t','i','k'),('t','i','g'),('t','o','p'),('t','o','b'),('t','o','t'),('t','o','d'),('t','o','k'),('t','o','g'),('t','u','p'),('t','u','b'),('t','u','t'),('t','u','d'),('t','u','k'),('t','u','g'),('d','a','p'),('d','a','b'),('d','a','t'),('d','a','d'),('d','a','k'),('d','a','g'),('d','e','p'),('d','e','b'),('d','e','t'),('d','e','d'),('d','e','k'),('d','e','g'),('d','i','p'),('d','i','b'),('d','i','t'),('d','i','d'),('d','i','k'),('d','i','g'),('d','o','p'),('d','o','b'),('d','o','t'),('d','o','d'),('d','o','k'),('d','o','g'),('d','u','p'),('d','u','b'),('d','u','t'),('d','u','d'),('d','u','k'),('d','u','g'),('k','a','p'),('k','a','b'),('k','a','t'),('k','a','d'),('k','a','k'),('k','a','g'),('k','e','p'),('k','e','b'),('k','e','t'),('k','e','d'),('k','e','k'),('k','e','g'),('k','i','p'),('k','i','b'),('k','i','t'),('k','i','d'),('k','i','k'),('k','i','g'),('k','o','p'),('k','o','b'),('k','o','t'),('k','o','d'),('k','o','k'),('k','o','g'),('k','u','p'),('k','u','b'),('k','u','t'),('k','u','d'),('k','u','k'),('k','u','g'),('g','a','p'),('g','a','b'),('g','a','t'),('g','a','d'),('g','a','k'),('g','a','g'),('g','e','p'),('g','e','b'),('g','e','t'),('g','e','d'),('g','e','k'),('g','e','g'),('g','i','p'),('g','i','b'),('g','i','t'),('g','i','d'),('g','i','k'),('g','i','g'),('g','o','p'),('g','o','b'),('g','o','t'),('g','o','d'),('g','o','k'),('g','o','g'),('g','u','p'),('g','u','b'),('g','u','t'),('g','u','d'),('g','u','k'),('g','u','g')]

