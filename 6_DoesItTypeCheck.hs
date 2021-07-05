--6_DoesItTypeCheck.hs
module DoesItTypeCheck where
import Data.List

data Person = Person Bool deriving Show  -- added: deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn $ "Printout of " ++ (show person)

data Mood = Blah
            | Woot deriving (Show, Eq)   -- added: Eq
settleDown :: Mood -> Mood    -- added line
settleDown x = if x == Woot
                then Blah
                else x

type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)
s1 :: Object -> Sentence     -- added line
s1 = Sentence "dogs" "drool"
s2 :: Sentence    -- added Line
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String deriving (Ord, Eq, Show)   -- added Ord to make comparePapus work
data Yeah =
    Yeah Bool deriving (Ord, Eq, Show)  -- added Ord to make comparePapus work
data Papu =
    Papu Rocks Yeah
    deriving (Ord, Eq, Show)   -- added Ord to make comparePapus work
-- phew = Papu "chases" True   -- does not compile - "chases" is not type Rocks and True is not type Yeah
truth :: Papu   -- added Line - not explicitly neccesary, but better
truth = Papu (Rocks "chomskydoz")
                (Yeah True)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
comparePapus :: Papu -> Papu -> Bool   -- no Ord for > defined - did not compile
comparePapus p p' = p > p'

i :: Num a => a
-- i :: a   -- this instead raises "no instance for (Num a) ..."
i = 1

f :: Float
--f :: Num a => a  -- this instead raises "Could not deduce (Fractional a)"
f = 1.0

g :: Float
--g :: Fractional a => a   -- works and defaults to Double
g = 1.0

h :: Float
--h :: RealFrac a => a   -- works and defaults to Double
h = 1.0

freud :: a -> a
--freud :: Ord a => a -> a   -- works, but Ord is defined without use of it
freud x = x

freud' :: a -> a
--freud' :: Int -> Int   -- works and is better definition when only working with Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
--sigmund :: a -> a   -- does not work - sigmund a won't fit to myX Int (set by 1  to Int)
sigmund x = myX

myX' = 1 :: Int
sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a  -- does not work " Could not deduce (a ~ Int)"
sigmund' x = myX'

jung :: Ord a => [a] -> a
--jung :: [Int] -> Int  -- works - if intended to use Int this is better
jung xs = head (sort xs)

young :: [Char] -> Char
--young :: Ord a => [a] -> a  -- this works for all lists not only String
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a  --  don't work "Could not deduce (a ~ Char)"'
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = aTob a == b

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith aTob itgr a = aTob a

--Examine the following code and decide whether it will type-
--check. Then load it in GHCi and see if you were correct. If
--it doesn’t typecheck, try to match the type error against your
--understanding of why it didn’t work. If you can, fix the error
--and re-run the code.

--1. Does the following code typecheck? If not, why not?
--data Person = Person Bool
--printPerson :: Person -> IO ()
--printPerson person = putStrLn (show person)
--2. Does the following typecheck? If not, why not?
--data Mood = Blah
--            | Woot deriving Show
--settleDown x = if x == Woot
--                then Blah
--                else x
--3. If you were able to get settleDown to typecheck:
--    a) What values are acceptable inputs to that function?
--    b) What will happen if you try to run settleDown 9? Why?
--    c) What will happen if you try to run Blah > Woot? Why?
--4. Does the following typecheck? If not, why not?
--type Subject = String
--type Verb = String
--type Object = String
--data Sentence =
--    Sentence Subject Verb Object
--    deriving (Eq, Show)
--s1 = Sentence "dogs" "drool"
--s2 = Sentence "Julie" "loves" "dogs"


--Given a datatype declaration, what can we do?
--Given the following datatype definitions:
--data Rocks =
--    Rocks String deriving (Eq, Show)
--data Yeah =
--    Yeah Bool deriving (Eq, Show)
--data Papu =
--    Papu Rocks Yeah
--    deriving (Eq, Show)

--Which of the following will typecheck? For the ones that
--don’t typecheck, why don’t they?
--1. phew = Papu "chases" True
--2. truth = Papu (Rocks "chomskydoz")
--                (Yeah True)
--3. equalityForall :: Papu -> Papu -> Bool
--   equalityForall p p' = p == p'
--4. comparePapus :: Papu -> Papu -> Bool
--   comparePapus p p' = p > p'

--We’re going to give you two types and their implementations.
--Then we’re going to ask you if you can substitute the second
--type for the first. You can test this by typing the first declaration
--and its type into a file and editing in the new one, loading to
--see if it fails. Don’t guess, test all your answers!
--1. For the following definition.
--a) i :: Num a => a
--i = 1
--b) Try replacing the type signature with the following:
--i :: a
--After you’ve formulated your own answer, then tested
--that answer and believe you understand why you were
--right or wrong, make sure to use GHCi to check what
--type GHC infers for the definitions we provide without
--a type assigned. For example, for this one, you’d type
--in:
--Prelude> let i = 1
--Prelude> :t i
---- Result elided intentionally.
--2.
--a) f :: Float
--f = 1.0
--b) f :: Num a => a
--3.
--a) f :: Float
--f = 1.0
--b) f :: Fractional a => a
--4. Hint for the following: type :info RealFrac in your REPL.
--a) f :: Float
--f = 1.0
--b) f :: RealFrac a => a
--5.
--a) freud :: a -> a
--freud x = x
--b) freud :: Ord a => a -> a
--6.
--a) freud' :: a -> a
--freud' x = x
--b) freud' :: Int -> Int
--7.
--a) myX = 1 :: Int
--sigmund :: Int -> Int
--sigmund x = myX
--b) sigmund :: a -> a
--8.
--a) myX = 1 :: Int
--sigmund' :: Int -> Int
--sigmund' x = myX
--b) sigmund' :: Num a => a -> a
--9.
--a) You’ll need to import sort from Data.List.
--jung :: Ord a => [a] -> a
--jung xs = head (sort xs)
--b) jung :: [Int] -> Int
--10.
--a) young :: [Char] -> Char
--young xs = head (sort xs)
--b) young :: Ord a => [a] -> a
--11.
--a) mySort :: [Char] -> [Char]
--mySort = sort
--signifier :: [Char] -> Char
--signifier xs = head (mySort xs)
--b) signifier :: Ord a => [a] -> a

--Type-Kwon-Do Two: Electric Typealoo
--Round Two! Same rules apply — you’re trying to fill in terms
--(code) which’ll fit the type. The idea with these exercises is that
--you’ll derive the implementation from the type information.
--You’ll probably need to use stuff from Prelude.
--1. chk :: Eq b => (a -> b) -> a -> b -> Bool
--chk = ???
--2. -- Hint: use some arithmetic operation to
---- combine values of type 'b'. Pick one.
--arith :: Num b
--      => (a -> b)
--      -> Integer
--      -> a
--      -> b
--arith = ???
