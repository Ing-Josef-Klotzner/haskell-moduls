--12_5_ChapterExercises.hs
module ChapterExercises_12_5 where
import Data.Char
import Data.List
--import Control.Arrow

--Determine the kinds
--1. Given
--    id :: a -> a
--    What is the kind of a?
-- Answer: * (any type)
--2. r :: a -> f a
--    What are the kinds of a and f?
-- Answer: kind of a is * and f is like Maybe, so * -> *

--String processing

--    Because this is the kind of thing linguists ahem enjoy doing in
--    their spare time.
--1. Write a recursive function named replaceThe which takes a
--    text/string, breaks it into words and replaces each instance
--    of “the” with “a”. It’s intended only to replace exactly
--    the word “the”. notThe is a suggested helper function for
--    accomplishing this.
-- example GHCi session
-- above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

-- von Robert (unwords am beginn angefügt):
replaceThe'' :: String -> String
replaceThe'' = unwords . map (fromMaybe "a" . notThe'') . words
-- mit notThe:
notThe'' :: String -> Maybe String
notThe'' str = if map toLower str == "the" then Nothing else Just str

notThe :: String -> Maybe String
notThe x
    | x == "the" = Nothing
    | otherwise = Just x
-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe x = go (words x)
    where go w 
            | w == [] = []
            | length w == 1 = fromMaybe "a" $ notThe $ head w
            | otherwise = (fromMaybe "a" $ notThe $ head w) ++ " " ++ go (tail w)

fromMaybe :: a -> Maybe a -> a
fromMaybe d (Just x) = x
fromMaybe d Nothing = d

replaceThe' :: String -> String
replaceThe' [] = []
replaceThe' x 
    | notThe (headWord x) == Nothing && tailWords x /= [] = "a" ++ " " ++ replaceThe' (tailWords x)
    | notThe (headWord x) == Nothing = "a" ++ replaceThe' (tailWords x)
    | notThe (headWord x) == Just (headWord x) && tailWords x /= [] = headWord x ++ " " ++ replaceThe' (tailWords x)
    | otherwise = headWord x ++ replaceThe' (tailWords x)
headWord :: String -> String
headWord [] = []
headWord x 
    | head x /= ' ' = [head x] ++ headWord (tail x)
    | otherwise = ""
tailWords :: String -> String
tailWords [] = []
tailWords x
    | headWord x == "" = tail x
    | head x == head (headWord x ++ " ") = tailWords (tail x)

--2. Write a recursive function that takes a text/string, breaks
--    it into words, and counts the number of instances of ”the”
--    followed by a vowel-initial word.
-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel x = go (words x) 0
    where go w i
            | w == [] = i
            | length w == 1 = i
            | head w == "the" && (head $ head $ tail w) `elem` vowels = go (tail w) (i + 1)
            | otherwise = go (tail w) i

--3. Return the number of letters that are vowels in a word.
--    Hint: it’s helpful to break this into steps. Add any helper
--    functions necessary to achieve your objectives.
--        a) Test for vowelhood
--        b) Return the vowels of a string
--        c) Count the number of elements returned
-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels w = go w 0
    where go s i
            | s == [] = i
            | length s == 1 && head s `elem` vowels = i + 1
            | length s == 1 = i
            | head s `elem` vowels = go (tail s) (i + 1)
            | otherwise = go (tail s) i

--Validate the word
--Use the Maybe type to write a function that counts the number
--of vowels in a string and the number of consonants. If the
--number of vowels exceeds the number of consonants, the
--function returns Nothing. In many human languages, vowels
--rarely exceed the number of consonants so when they do, it
--may indicate the input isn’t a word (that is, a valid input to your
--dataset):
newtype Word' = Word' String deriving (Eq, Show)
vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord s = go (cVaC s)
    where go tupVC
            | fst tupVC > snd tupVC = Nothing
            | otherwise = Just (Word' s)
-- count vowels and consonants
cVaC :: (Num a, Num a1) => [Char] -> (a, a1)
cVaC w = go w (0,0)
    where go s (v,c)
            | s == [] = (v,c)
            | length s == 1 && head s `elem` vowels = (v + 1, c)
            | length s == 1 = (v, c + 1)
            | head s `elem` vowels = go (tail s) ((v + 1), c)
            | otherwise = go (tail s) (v, (c + 1))

--It’s only Natural
--You’ll be presented with a datatype to represent the natural
--numbers. The only values representable with the naturals
--are whole numbers from zero to infinity. Your task will be
--to implement functions to convert Naturals to Integers and
--Integers to Naturals. The conversion from Naturals to Integers
--won’t return Maybe because Integer is a strict superset of Natural.
--Any Natural can be represented by an Integer, but the same is
--not true of any Integer. Negative numbers are not valid natural
--numbers.

-- As natural as any
-- competitive bodybuilder
data Nat = Zero | Succ Nat deriving (Eq, Show)
-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = succ (natToInteger x)
-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat x 
    | x < 0 = Nothing
    | otherwise = Just (go x)
        where go i
                | i == 0 = Zero
                | i > 0 = Succ $ go (i - 1)

--Small library for Maybe
--Write the following functions. This may take some time.
--1. Simple boolean checks for Maybe values.
-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' Nothing = False 

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' (Just _) = False
--2. The following is the Maybe catamorphism. You can turn a
--    Maybe value into anything else with this.
-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee t f Nothing = t
mayybee t f (Just x) = f x

--3. In case you just want to provide a fallback value.
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
-- already solved above in this script
--fromMaybe :: a -> Maybe a -> a
--fromMaybe d (Just x) = x
--fromMaybe d Nothing = d

-- Try writing it in terms
-- of the maybe catamorphism

--4. Converting between List and Maybe.
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x
-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

--5. For when we want to drop the Nothing values from our list.
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : ms) = catMaybes ms
catMaybes (Just m : ms) = m : catMaybes ms
--6. You’ll see this called “sequence” later.
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe lms = if nothingIn lms then Nothing else Just (go lms)
        where   go [] = []
                go (Nothing : ms) = go ms
                go (Just m : ms) = m : go ms
                nothingIn [] = False
                nothingIn (Nothing:ms) = True || nothingIn ms
                nothingIn (Just m : ms) = False || nothingIn ms

flipMaybe' :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe' [] = Nothing
flipMaybe' lms = if elem Nothing lms then Nothing else Just (go lms)
        where   go [] = []
                go (Nothing : ms) = go ms
                go (Just m : ms) = m : go ms
--Small library for Either
--Write each of the following functions. If more than one possi-
--ble unique function exists for the type, use common sense to
--determine what it should do.
--1. Try to eventually arrive at a solution that uses foldr, even
--  if earlier versions don’t use foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
    where   f (Left y) z = ((:) y z)
            f (Right y) z = z
--2. Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
rights' = foldr f []
    where   f (Right y) z = ((:) y z)
            f (Left y) z = z
--3. 
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = (lefts' x, rights' x)
--4. 
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' f (Left x) = Nothing
--5. This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa fb (Left x) = fa x
either' fa fb (Right x) = fb x
-- *ChapterExercises_12_5> either' toUpper toLower (Right 'R')
--'r'
-- *ChapterExercises_12_5> either' toUpper toLower (Left 'd')
--'D'

--6. Same as before, but use the either' function you just
--wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (Just . f)   -- (statt '\x -> Nothing' auch 'const Nothing')
--Most of the functions you just saw are in the Prelude, Data.Maybe,
--or Data.Either but you should strive to write them yourself
--without looking at existing implementations. You will deprive
--yourself if you cheat.

--Why bother?
--We bother with this for the same reason we abstracted direct
--recursion into folds, such as with sum, product, and concat.
--import Data.List
mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
    where   go :: Num a => a -> [a] -> a
            go n [] = n
            go n (x:xs) = (go (n+x) xs)
niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0
mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
    where   go :: Num a => a -> [a] -> a
            go n [] = n
            go n (x:xs) = (go (n*x) xs)
niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

--Remember the redundant structure when we looked at
--folds?
mehConcat :: [[a]] -> [a]
mehConcat xs =  go [] xs
        where   go :: [a] -> [[a]] -> [a]
                go xs' [] = xs'
                go xs' (x:xs) = (go (xs' ++ x) xs)
niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []
--This may have given you a mild headache, but you may
--also see that this same principle of abstracting out common
--patterns and giving them names applies as well to unfolds as
--it does to folds.
--Write your own iterate and unfoldr
--1. Write the function myIterate using direct recursion. Com-
--    pare the behavior with the built-in iterate to gauge cor-
--    rectness. Do not look at the source or any examples of
--    iterate so that you are forced to do this yourself.
-- iterate is infinite !! 
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)
-- *ChapterExercises_12_5 Data.Char Data.Maybe> take 8 $ myIterate (+1) 0
--[0,1,2,3,4,5,6,7]
--2. Write the function myUnfoldr using direct recursion. Com-
--    pare with the built-in unfoldr to check your implementa-
--    tion. Again, don’t look at implementations of unfoldr so
--    that you figure it out yourself.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f y = go (f y) where
    go Nothing = []
    go (Just (a, b)) = a : myUnfoldr f b
-- *ChapterExercises_12_5 Data.Char Data.Maybe> take 8 $ myUnfoldr  (\b -> Just (b, b+1)) 0
--[0,1,2,3,4,5,6,7]
-- Roberts Lösungen:
myUnfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr' f = maybe [] buildList . f
  where
    buildList (a, b) = a : myUnfoldr' f b
myUnfoldr'' :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr'' f = maybe [] (\(a, b) -> a : myUnfoldr'' f b)  . f

myUnfoldr''' :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr''' f = maybe [] (uncurry (:) . fmap (myUnfoldr''' f)) . f
--3. Rewrite myIterate into betterIterate using myUnfoldr. A
--    hint — we used unfoldr to produce the same results as
--    iterate earlier. Do this with different functions and see if
--    you can abstract the structure out.
-- It helps to have the
-- types in front of you
-- myUnfoldr :: (b -> Maybe (a, b))
-- -> b
-- -> [a]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b)) -- myUnfoldr ...?
-- von Robert: (erfordert import Control.Arrow)
--betterIterate' :: (a -> a) -> a -> [a]
--betterIterate' f = myUnfoldr (Just . (id  &&& f))

--Remember, your betterIterate should have the same re-
--sults as iterate.

--Finally something other than a list!
--Given the BinaryTree from last chapter, complete the following
--exercises. Here’s that datatype again:
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)
--1. Write unfold for BinaryTree.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = go (f x) where
    go (Just (y,b,z)) = Node (unfold f y) b (unfold f z)
    go Nothing = Leaf
-- *ChapterExercises_12_5> unfold (\x -> if x > 0 then (Just(x-1,x,x-1)) else Nothing) 3
--Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 1 Leaf)) 3 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 1 Leaf))

--2. Make a tree builder.
--Using the unfold function you’ve made for BinaryTree, write
--the following function:
treeBuild :: (Num b, Ord b) => b -> BinaryTree b
--treeBuild o@n = unfold (\x -> if x < o then (Just(x + 1, x, x + 1)) else Nothing) (o - n)
treeBuild n = unfold (\x -> if x < n then (Just(x + 1, x, x + 1)) else Nothing) 0
--You should be producing results that look like the following:
--Prelude> treeBuild 0
--Leaf
--Prelude> treeBuild 1
--Node Leaf 0 Leaf
--Prelude> treeBuild 2
--Node (Node Leaf 1 Leaf)
--      0
--      (Node Leaf 1 Leaf)
--Prelude> treeBuild 3
--Node  (Node (Node Leaf 2 Leaf))
--            1
--            (Node Leaf 2 Leaf))
--      0
--      (Node (Node Leaf 2 Leaf)
--            1
--            (Node Leaf 2 Leaf))
--Or in a slightly different representation:
--     0
--     0
--   /   \
--   1   1
--   0
-- /   \
-- 1   1
-- /\  /\
-- 2 2 2 2

-- <?xml version="1.0" encoding="UTF-8" standalone="no"?>
-- <svg width="210mm" height="297mm" version="1.1"
--    xmlns="http://www.w3.org/2000/svg">
--    <desc>Just a 0 node for test</desc>
--    <text style="font-size:160px;fill:#000000;font-family:Sans" x="340" y="140">1</text>
--    <text style="font-size:160px;fill:#000000;font-family:Sans" x="280" y="300">/</text>
--    <text style="font-size:160px;fill:#000000;font-family:Sans" x="448" y="300">\</text>
-- </svg>
createdBy = "Created by Josef Klotzner (printBinaryTree, Haskell, svg)"
string1 = map chr [65..126]
string2 = map chr [160..250]
svg_head = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++
            "<!-- Created by printBinaryTree (Haskell, Josef Klotzner) -->\n" ++
            "<svg width=\"210mm\" height=\"297mm\" version=\"1.1\"\n" ++
            "   xmlns=\"http://www.w3.org/2000/svg\">\n" ++
            "   <text style=\"font-size:8px;fill:#000000;font-family:Sans\" \n" ++ 
            "   x=\"40\" y=\"40\">" ++ createdBy ++ "</text>\n"
-- only to show various characters of ascii table
svg_head1 = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++
            "<!-- Created by printBinaryTree (Haskell, Josef Klotzner) -->\n" ++
            "<svg width=\"210mm\" height=\"297mm\" version=\"1.1\"\n" ++
            "   xmlns=\"http://www.w3.org/2000/svg\">\n" ++
            "   <text style=\"font-size:8px;fill:#000000;font-family:Sans\" \n" ++ 
            "   x=\"40\" y=\"40\">" ++ string1 ++ "</text>\n" ++
            "   <text style=\"font-size:16px;fill:#000000;font-family:Sans\" \n" ++ 
            "   x=\"40\" y=\"80\">" ++ string2 ++ "</text>\n"
svg_char :: (Show a, Show a1, Show a2) => a -> a1 -> a2 -> [Char] -> [Char]
svg_char fSz x y c = "   <text style=\"font-size:" ++ show fSz ++ "px;fill:#000000;font-family:Sans\"" ++
            " x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\">" ++ c ++ "</text>\n"
svg_node :: (Fractional a, Show a) => a -> a -> a -> [Char] -> [Char]
svg_node fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\")
-- a node plus next node root positions with leafs
--     x
--    / \
--   x   x
svg_node_plus fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\") ++
                    (svg_char (fSz / 2) (x - 80 / 160 * fSz) (y + fSz * 1.6) "x") ++
                    (svg_char (fSz / 2) (x + 130 / 160 * fSz) (y + fSz * 1.6) "x")
svg_node_ll fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\") ++
                    (svg_char (fSz / 2) (x - 80 / 160 * fSz) (y + fSz * 1.6) "x")
svg_node_rl fSz x y c = (svg_char fSz x y c) ++ 
                    (svg_char fSz (x - 60 / 160 * fSz) (y + fSz) "/") ++ 
                    (svg_char fSz (x + 108 / 160 * fSz) (y + fSz) "\\") ++
                    (svg_char (fSz / 2) (x + 130 / 160 * fSz) (y + fSz * 1.6) "x")
svg_leaf fSz x y = (svg_char fSz x y "x") 
svg_tail = "</svg>"
filename = "./BinaryTreePrint.svg"
filename1 = "./test.svg"
rtFSz = 160
rootX = 340
rootY = 140
rootLvl = 0
--write_svg = writeFile filename1 (svg_head ++ (svg_node rtFSz rootX rootY "0") ++ svg_tail)
write_svg = writeFile filename1 (svg_head1 ++ (svg_node rtFSz rootX rootY "") ++ svg_tail)
printBinaryTree :: Show a => BinaryTree a -> IO ()
printBinaryTree tree = writeFile filename (svg_head ++ 
                draw_tree (graph_tree rtFSz rootX rootY rootLvl tree) ++ svg_tail)
-- test to show the leafs:
--  printBinaryTree $ insert' 1 $ insert' 3 $ insert' (-3) $ insert' (-2) $ insert' (-1) $ insert' 4 $ insert' 2 $ treeBuild 1
--Good work.

graph_tree :: (Fractional t, Num t1) => t -> t -> t -> t1 -> BinaryTree t2 -> BinaryTree (t, t, t, t1, t2)
graph_tree fSz x y tLvl Leaf = Leaf
graph_tree fSz x y tLvl (Node Leaf a Leaf) = Node Leaf (fSz, x, y, tLvl, a) Leaf
graph_tree fSz x y tLvl (Node left a Leaf) = Node (graph_tree (fSz / 2) (x - 80 / 160 * fSz)
                                            (y + fSz * 1.6) (tLvl + 1) left) (fSz, x, y, tLvl, a) Leaf
graph_tree fSz x y tLvl (Node Leaf a right) = Node Leaf (fSz, x, y, tLvl, a) (graph_tree (fSz / 2) 
                                            (x + 130 / 160 * fSz) (y + fSz * 1.6) (tLvl + 1) right)
graph_tree fSz x y tLvl (Node left a right) = Node (graph_tree (fSz / 2) (x - 80 / 160 * fSz)
                                            (y + fSz * 1.6) (tLvl + 1) left) (fSz, x, y, tLvl, a) 
                                            (graph_tree (fSz / 2) 
                                            (x + 130 / 160 * fSz) (y + fSz * 1.6) (tLvl + 1) right)
node = treeBuild 7
node1 = insert' 7 node
node2 = insert' 8 node1
node3 = insert' 9 node2
test = printBinaryTree node3

draw_tree :: (Fractional a, Show a, Show a1) => BinaryTree (a, a, a, t, a1) -> [Char]
draw_tree Leaf = svg_leaf rtFSz rootX rootY
draw_tree (Node Leaf (fSz, x, y, tLvl, a) Leaf) = svg_node_plus fSz x y (show a)
draw_tree (Node left (fSz, x, y, tLvl, a) Leaf) = draw_tree left ++ svg_node_rl fSz x y (show a)
draw_tree (Node Leaf (fSz, x, y, tLvl, a) right) = svg_node_ll fSz x y (show a) ++ draw_tree right
draw_tree (Node left (fSz, x, y, tLvl, a) right) = draw_tree left ++ svg_node fSz x y (show a) ++ draw_tree right



insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

