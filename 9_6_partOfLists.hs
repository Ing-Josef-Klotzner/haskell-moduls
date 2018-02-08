--module PoemLines where

--Using takeWhile and dropWhile, write a function that takes a
--string and returns a list of strings, using spaces to separate
--the elements of the string into words, as in the following
--sample:
--Prelude> myWords "sheryl wants fun"
--["wallfish", "wants", "fun"]

strg = "sheryl wants fun"

strgToList :: String -> [String]
strgToList x = strgToList' x ' '
--strgToList x = go x []
--    where go s lst
--            | s == "" = lst
--            | otherwise = go (drop 1 $ dropWhile (/=' ') s) (lst ++ [(takeWhile (/=' ') s)])

--Next, write a function that takes a string and returns a list
--of strings, using newline separators to break up the string
--as in the following (your job is to fill in the undefined
--function):
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines x = strgToList' x '\n'
--myLines x = go x []
--    where go s lst
--            | s == "" = lst
--            | otherwise = go (drop 1 $ dropWhile (/='\n') s) (lst ++ [(takeWhile (/='\n') s)])

-- What we want 'myLines sentences'
-- to equal
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

--Now let’s look at what those two functions have in com-
--mon. Try writing a new function that parameterizes the
--character you’re breaking the string argument on and
--rewrite myWords and myLines using it.

strgToList' :: String -> Char -> [String]
strgToList' x sep = go x []
    where go s lst
            | s == "" = lst
            | otherwise = go (drop 1 $ dropWhile (/=sep) s) (lst ++ [(takeWhile (/=sep) s)])
            
-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = putStrLn $ "Are they equal? " ++ show (myLines sentences == shouldEqual) ++ "\n" ++
            "myLines sentences:\n" ++ show (myLines sentences) ++ "\n" ++
            "shouldEqual:\n" ++ show (shouldEqual)
