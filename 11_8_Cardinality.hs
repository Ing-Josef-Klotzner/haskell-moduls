-- 11_8_Cardinality.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Cardinality where
import Data.Int

--While we haven’t explicitly described the rules for calculating
--the cardinality of datatypes yet, you might already have an idea
--of how to do it for simple datatypes with nullary constructors.
--Try not to overthink these exercises — follow your intuition
--based on what you know.

--1. 
data PugType = PugData
-- Answer: Cardinality 1

--2. For this one, recall that Bool is also defined with the |:
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving Show
-- Answer: Cardinality 3

--3. Given what we know about Int8, what’s the cardinality ofInt16?
cardInt16 = fromIntegral (maxBound :: Int16) + 1 - fromIntegral (minBound :: Int16)
-- Answer: 65536  (=2^16)

--4. Use the REPL and maxBound and minBound to examine Int
--and Integer. What can you say about the cardinality of
--those types?

-- Cardinality of Int:
cardInt = fromIntegral (maxBound :: Int) + 1 - fromIntegral (minBound :: Int)
-- Answer: 18446744073709551616   (=2^64)
-- Cardinality of Integer is basically infinite, but limited by memory space of computer

--5. Extra credit (impress your friends!): What’s the connec-
--tion between the 8 in Int8 and that type’s cardinality of 256?
-- Answer: Cardinality of Int8 is 2^8

-- Exercises: For Example
data Example = MakeExample deriving Show

--1. What is the type of data constructor MakeExample? What
--happens when you request the type of Example?

-- *Cardinality Data.Int> :t MakeExample 
-- MakeExample :: Example

-- *Cardinality Data.Int> :t Example 
-- <interactive>:1:1: Not in scope: data constructor `Example'


--2. What if you try :info on Example in GHCi? Can you deter-
--mine what typeclass instances are defined for the Example
--type using :info in GHCi?

-- *Cardinality Data.Int> :i Example 
-- data Example = MakeExample 	-- Defined at 11_8_Cardinality.hs:37:6
-- instance Show Example -- Defined at 11_8_Cardinality.hs:37:37


--3. Try making a new datatype like Example but with a single
--type argument added to MakeExample, such as Int. What has
--changed when you query MakeExample with :type in GHCi?

data Example_int = MakeExample_Int Int deriving Show

-- *Cardinality Data.Int> :t MakeExample_Int 
-- MakeExample_Int :: Int -> Example_int


-- 11_9_newtype

tooManyGoats' :: Int -> Bool
tooManyGoats' n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show,TooMany)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where 
    tooMany :: a -> Bool
instance TooMany Int where 
    tooMany n = n > 42
-- following instances not necessary with {-# LANGUAGE GeneralizedNewtypeDeriving #-} 
-- and TooMany entry in deriving
--instance TooMany Goats where
--    tooMany (Goats n) = n > 43
--instance TooMany Cows where
--    tooMany (Cows n) = n > 43

-- *Cardinality Data.Int> tooMany (Goats 43)
-- True


--Exercises: Logic Goats

--1. Reusing the TooMany typeclass, write an instance of the
--typeclass for the type (Int, String). This will require
--adding a language pragma named FlexibleInstances (5) if
--you do not use a newtype — GHC will tell you what to do.
-- (5)
--https://ghc.haskell.org/trac/haskell-prime/wiki/FlexibleInstances
instance TooMany (Int, String) where
    tooMany (i, s) = tooMany (i + length s)
-- test in REPL:
-- tooMany (37::Int,"string")

--2. Make another TooMany instance for (Int, Int). Sum the
--values together under the assumption this is a count of
--goats from two fields.
instance TooMany (Int, Int) where
    tooMany (i, i1) = tooMany (i + i1)
-- test in REPL:
-- tooMany (37::Int,6::Int)

--3. Make another TooMany instance, this time for (Num a, TooMany
--a) => (a, a). This can mean whatever you want, such as
--summing the two numbers together.
instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (a, a1) = tooMany (a + a1)


-- 11_10_Exercises: Pity the Bool

--Given a datatype
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
--What is the cardinality of this datatype?
--2 ( like Bool)

--Given a datatype
-- bring Int8 in scope
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
myNumba = Numba (-128)
-- What is the cardinality of NumberOrBool?
-- It is 257   = 2^8 (Int8) + 1 (BoolyBool)

--What happens if you try to create a Numba with a numeric literal
--larger than 127? And with a numeric literal smaller than (-128)?
-- Answer:
-- *Cardinality Data.Int> let myNumba3 = Numba 128
-- *Cardinality Data.Int> myNumba3
-- Numba (-128)
-- *Cardinality Data.Int> let myNumba3 = Numba 129
-- *Cardinality Data.Int> myNumba3
-- Numba (-127)
-- *Cardinality Data.Int> let myNumba3 = Numba (-129)
-- *Cardinality Data.Int> myNumba3
-- Numba 127
-- *Cardinality Data.Int> let myNumba3 = Numba (-130)
-- *Cardinality Data.Int> myNumba3
-- Numba 126


-- Record Syntax

data Person =
    Person { name :: String
    , age :: Int }
    deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

-- *Cardinality Data.Int> name ca
-- "chris"
-- *Cardinality Data.Int> age ca
-- 16
-- *Cardinality Data.Int> name jm
-- "julie"
-- *Cardinality Data.Int> age jm
-- 108


-- 11_12_Exercises: How Does Your Garden Grow?

--data FlowerType = Gardenia
--    | Daisy
--    | Rose
--    | Lilac
--    deriving Show
type Gardener = String
--data Garden = Garden Gardener FlowerType deriving Show

--What is the sum of products normal form of Garden?

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show


--11_13_Exercise: Programmers
--Write a function that generates all possible values of Programmer.
--Use the provided lists of inhabitants of OperatingSystem and
--ProgLang.

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
            , lang :: ProgLang }
            deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]


--11_14 Function type is exponential

data Quantum =
    Yes
    | No
    | Both
    deriving (Eq, Show)

--According to the equality of a -> b and b u^a there should be 2^3
--or 8 implementations of this function. Does this hold? Write
--it out and prove it for yourself.

-- Implementation 1
convert1 :: Quantum -> Bool
convert1 (Yes) = True
convert1 (No) = True
convert1 (Both) = True
-- Implementation 2
convert2 :: Quantum -> Bool
convert2 (Yes) = True
convert2 (No) = True
convert2 (Both) = False
-- Implementation 3
convert3 :: Quantum -> Bool
convert3 (Yes) = True
convert3 (No) = False
convert3 (Both) = True
-- Implementation 4
convert4 :: Quantum -> Bool
convert4 (Yes) = True
convert4 (No) = False
convert4 (Both) = False
-- Implementation 5
convert5 :: Quantum -> Bool
convert5 (Yes) = False
convert5 (No) = True
convert5 (Both) = True
-- Implementation 6
convert6 :: Quantum -> Bool
convert6 (Yes) = False
convert6 (No) = True
convert6 (Both) = False
-- Implementation 7
convert7 :: Quantum -> Bool
convert7 (Yes) = False
convert7 (No) = False
convert7 (Both) = True
-- Implementation 8
convert8 :: Quantum -> Bool
convert8 (Yes) = False
convert8 (No) = False
convert8 (Both) = False


-- Exercises: The Quad

--Determine how many unique inhabitants each type has.
--Suggestion: do the arithmetic unless you want to verify.
--Writing them out gets tedious quickly.
data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show)
-- how many different forms can this take?
-- Answer: this is sum type -> 4 + 4 = 8

eQuad1 :: Either Quad Quad
eQuad1 = Left One

eQuad2 :: Either Quad Quad
eQuad2 = Left Two

eQuad3 :: Either Quad Quad
eQuad3 = Left Three

eQuad4 :: Either Quad Quad
eQuad4 = Left Four

eQuad5 :: Either Quad Quad
eQuad5 = Right One

eQuad6 :: Either Quad Quad
eQuad6 = Right Two

eQuad7 :: Either Quad Quad
eQuad7 = Right Three

eQuad8 :: Either Quad Quad
eQuad8 = Right Four

--2. 
--Answer: 4 * 4 = 16
prodQuad1 :: (Quad, Quad)
prodQuad1 = (One,One)
-- .
-- .
prodQuad16 :: (Quad, Quad)
prodQuad16 = (Four,Four)

--3. 
--Answer: 4 ^ 4 = 256
funcQuad1 :: Quad -> Quad
funcQuad1 (One) = One    -- (or Two or Three or Four)
funcQuad1 (Two) = One
funcQuad1 (Three) = One
funcQuad1 (Four) = One
-- .
-- .
funcQuad256 :: Quad -> Quad
funcQuad256 (One) = Four
funcQuad256 (Two) = Four
funcQuad256 (Three) = Four
funcQuad256 (Four) = Four
-- easily to see, 4 * 4 * 4 * 4, makes 256 possibilities

--4.
--Answer: 2 * 2 * 2 = 8
prodTBool1 :: (Bool, Bool, Bool)
prodTBool1 = (False,False,False)
-- .
-- .
prodTBool8 :: (Bool, Bool, Bool)
prodTBool8 = (True,True,True)

--5.
--Answer: 2 ^ 2 ^ 2 = 16
gTwo1 :: Bool -> Bool -> Bool
gTwo1 False False = False
gTwo1 False True = False
gTwo1 True False = False
gTwo1 True True = False
-- .
-- .
gTwo16 :: Bool -> Bool -> Bool
gTwo16 False False = True
gTwo16 False True = True
gTwo16 True False = True
gTwo16 True True = True

--6. Hint: 5 digit number
--Answer: 4 ^ 4 ^ 2 = 65536
fTwo1 :: Bool -> Quad -> Quad
fTwo1 False (One) = One     --(or Two, or Three, or Four)
fTwo1 True (One) = One
fTwo1 False (Two) = One
fTwo1 True (Two) = One
fTwo1 False (Three) = One
fTwo1 True (Three) = One
fTwo1 False (Four) = One
fTwo1 True (Four) = One
-- .
-- .
fTwo65536 :: Bool -> Quad -> Quad
fTwo65536 False (One) = Four
fTwo65536 True (One) = Four
fTwo65536 False (Two) = Four
fTwo65536 True (Two) = Four
fTwo65536 False (Three) = Four
fTwo65536 True (Three) = Four
fTwo65536 False (Four) = Four
fTwo65536 True (Four) = Four
-- 4 ^ 8 = 65536


-- 11_17_Binary Trees

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a
    => a
    -> BinaryTree a
    -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

t1 = insert' 0 Leaf
-- *Cardinality Data.Int> t1
-- Node Leaf 0 Leaf
t2 = insert' 3 t1
-- *Cardinality Data.Int> t2
-- Node Leaf 0 (Node Leaf 3 Leaf)
t3 = insert' 5 t2
-- *Cardinality Data.Int> t3
-- Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf))
t4 = insert' (-1) t3
-- *Cardinality Data.Int> t4
-- Node (Node Leaf (-1) Leaf) 0 (Node Leaf 3 (Node Leaf 5 Leaf))

--Write map for Binary Tree

mapTree :: (a -> b)
    -> BinaryTree a
    -> BinaryTree b
mapTree _ Leaf = Leaf
--mapTree f (Node left a right) =
--    Node undefined undefined undefined
-- Exercise:
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
    Node (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO ()
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

--Prelude> :t Node
--Node :: BinaryTree a
--    -> a
--    -> BinaryTree a
--    -> BinaryTree a
--So you need to pass it more BinaryTree, a single value, and
--more BinaryTree. You have the following terms available to
--you:
--1. f :: (a -> b)
--2. left :: BinaryTree a
--3. a :: a
--4. right :: BinaryTree a

--Now the Node return needs to have a value of type b and
--BinaryTree values with type b inside them. You have two func-
--tions at your disposal. One gets you (a -> b), the other maps
--BinaryTrees of type a into BinaryTrees of type b. Get ’em tiger.
--A few suggestions that might help you with this exercise.
--1. Split out the patterns your function should match on first.
--2. Implement the base case first.
--3. Try manually writing out the steps of recursion at first,
--then collapse them into a single step that is recursive

-- see solution of mapTree function above

-- *Cardinality Data.Int> mapOkay 
-- "yup okay!"

-- test with changing types:
-- *Cardinality Data.Int> t3
-- Node Leaf 0 (Node Leaf 3 (Node Leaf 5 Leaf))
-- *Cardinality Data.Int> mapTree (\x -> if x == 3 then True else False) t3
-- Node Leaf False (Node Leaf True (Node Leaf False Leaf))


--Convert binary trees to lists
--Write functions to convert BinaryTree values to lists. Make
--certain your implementation passes the tests.

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]
testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)
testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Preorder failed check"
testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Inorder failed check"
testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

--Write foldr for BinaryTree
--Given the definition of BinaryTree we have provided, write a
--catamorphism for the binary trees.
-- any traversal order is fine
-- foldr f z (x:xs) = f x (foldr f z xs)
-- preorder
foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ b Leaf = b
-- reversed preorder
-- foldTree' f b (Node left a right) = foldTree' f (foldTree' f (f a b) left) right
foldTree' f b (Node left a right) = f a (foldTree' f (foldTree' f b right) left)
-- inorder
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
-- reversed inorder
-- foldTree f b (Node left a right) = foldTree f (f a (foldTree f b left)) right
foldTree f b (Node left a right) = foldTree f (f a (foldTree f b right)) left
-- postorder
foldTree'' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree'' _ b Leaf = b
-- reversed postorder
-- foldTree'' f b (Node left a right) = f a (foldTree'' f (foldTree'' f b left) right)
foldTree'' f b (Node left a right) = foldTree'' f (foldTree'' f (f a b) right) left
-- *Cardinality Data.Int> foldTree (+) 0 t3
-- 8

testFoldTreePreorder :: IO ()
testFoldTreePreorder =
    if preorder testTree == foldTree' (:) [] testTree
    then putStrLn "FoldTree Preorder fine!"
    else putStrLn "FoldTree Preorder failed check"

testFoldTreeInorder :: IO ()
testFoldTreeInorder =
    if inorder testTree == foldTree (:) [] testTree
    then putStrLn "FoldTree Inorder fine!"
    else putStrLn "FoldTree Inorder failed check"

testFoldTreePostorder :: IO ()
testFoldTreePostorder =
    if postorder testTree == foldTree'' (:) [] testTree
    then putStrLn "FoldTree Postorder fine!"
    else putStrLn "FoldTree Postorder failed check"

