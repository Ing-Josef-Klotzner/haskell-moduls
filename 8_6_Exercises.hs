-- 8_6_Exercises.hs
module Exercises8 where
import Data.Bits (shift, testBit)

func :: [a] -> [a] -> [a]
func x y = x ++ y

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny
appedCatty :: String -> String
appedCatty = cattyConny "woops"
frappe :: String -> String
frappe = flippy "haha"

data DividedResult =
    Result Integer Integer
    | DividedByZero deriving Show

dividedBy''' :: Integer -> Integer -> DividedResult
dividedBy''' num 1 = Result num 0
dividedBy''' _ 0 = DividedByZero
dividedBy''' 0 _ = Result 0 0
dividedBy''' num denom = go num denom 0
    where go n d count
            | n < d && n >= 0 && d > 0    = Result count n
            | n > 10000000000 * d && n >= 0 && d > 0 = go (n - d * 10000000000) d (count + 10000000000)
            | n > 100000 * d && n >= 0 && d > 0 = go (n - d * 100000) d (count + 100000)
            | n >= d && n >= 0 && d > 0    = go (n - d) d (count + 1)
            | n * (-1) < d  && n <= 0 && d > 0 = Result count n
            | n * (-1) >= d * 10000000000  && n <= 0 && d > 0 = go (n + d * 10000000000) d (count - 10000000000)
            | n * (-1) >= d * 100000  && n <= 0 && d > 0 = go (n + d * 100000) d (count - 100000)
            | n * (-1) >= d  && n <= 0 && d > 0 = go (n + d) d (count - 1)
            | n * (-1) < d * (-1) && n <= 0 && d < 0 = Result count n
            | n * (-1) >= d * (-1 * 10000000000) && n <= 0 && d < 0 = go (n - d * 10000000000) d (count + 10000000000)
            | n * (-1) >= d * (-1 * 100000) && n <= 0 && d < 0 = go (n - d * 100000) d (count + 100000)
            | n * (-1) >= d * (-1) && n <= 0 && d < 0 = go (n - d) d (count + 1)
            | n < d * (-1) && n >= 0 && d < 0 = Result count n
            | n >= d * (-1 * 10000000000) && n >= 0 && d < 0 = go (n + d * 10000000000) d (count - 10000000000)
            | n >= d * (-1 * 100000) && n >= 0 && d < 0 = go (n + d * 100000) d (count - 100000)
            | otherwise = go (n + d) d (count - 1)

-- schokotupfen
-- nougatcreme
-- haselnuss
-- karamelschale
toffifee = "karamelschale" ++ " haselnuss " ++ " nougatcreme " ++ " schokotupfen"
-- *Exercises8> quotRem (-3)(-2)
--(1,-1)
-- *Exercises8> quotRem (3)(-2)
--(-1,1)
-- *Exercises8> quotRem (-3)(2)
--(-1,-1)

dividedBy :: (Integral b, Read b, Show b) => b -> b -> (b, b)
dividedBy x y
    | y == 0 = (0,0)
    | y < 0 && x < 0 = (fst $ dividedBy'' (abs x) (abs y), -(snd $ dividedBy'' (abs x) (abs y)))
    | y < 0 && x > 0 = (-(fst $ dividedBy'' x (abs y)), snd $ dividedBy'' x (abs y))
    | y > 0 && x < 0 = (-(fst $ dividedBy'' (abs x) y), -(snd $ dividedBy'' (abs x) y))
    | otherwise = dividedBy'' x y
    

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
    where go n
            d count
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

-- find potens of highest digit of result
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

-- version using purely bit shifting
mulIntS :: Integer -> Integer -> Integer
mulIntS x y
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs y < abs x = - (mulIntS'' (abs x) (abs y))
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs x < abs y = - (mulIntS'' (abs y) (abs x))
    | y < x = mulIntS'' (abs x) (abs y)
    | otherwise = mulIntS'' (abs y) (abs x)

mulIntS'' :: Integer -> Integer -> Integer
mulIntS'' x y = go 0 where
    go cnt
        | shift 1 cnt > y = 0
        | testBit y cnt = shift x cnt + go (cnt + 1)
        | otherwise = 0 + go (cnt + 1)


-- mulIntEthiopic:
mulIntEthiopic :: Integer -> Integer -> Integer
mulIntEthiopic x y
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs y < abs x = - (mulIntEthiopic'' (abs x) (abs y))
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs x < abs y = - (mulIntEthiopic'' (abs y) (abs x))
    | y > x = mulIntEthiopic'' (abs x) (abs y)
    | otherwise = mulIntEthiopic'' (abs y) (abs x)
 
mulIntEthiopic'' :: Integer -> Integer -> Integer
mulIntEthiopic'' a b =
    sum $
    map snd $
    filter (odd . fst) $
    zip (takeWhile (>= 1) $ iterate halve a) (iterate double b) where
        halve :: Integer -> Integer
        halve i = shift i (-1)
         
        double :: Integer -> Integer
        double i = shift i 1

productOf :: Integer -> Integer -> Integer
productOf 0 _ = 0
productOf _ 0 = 0
productOf x 1 = x
productOf 1 y = y
productOf x 2 = shift x 1
productOf x 3 = x + shift x 1
productOf x 4 = shift x 2
productOf x 5 = x + shift x 2
productOf x 6 = (shift x 1) + shift x 2
productOf x 7 = x + (shift x 1) + shift x 2
productOf x 8 = shift x 3
productOf x 9 = x + shift x 3
productOf x y = x + productOf x (y - 1)

mulInt :: Integer -> Integer -> Integer
mulInt x y
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs y < abs x = - (mulInt'' (abs x) (abs y))
    | (y > 0 && x < 0) || (y < 0 && x > 0) && abs x < abs y = - (mulInt'' (abs y) (abs x))
    | y < x = mulInt'' (abs x) (abs y)
    | otherwise = mulInt'' (abs y) (abs x)

mulInt'' :: Integer -> Integer -> Integer
mulInt'' x y = go x y 0
    where go a b res
            | b == 0 = res
            | otherwise = go (mulIntx10 a) (div b 10) (res + productOf a (mod b 10))

-- multiply Integer x by 10
mulIntx10 :: Integer -> Integer
mulIntx10 x = (shift x 1) + shift x 3

sumTo' :: (Eq a, Num a) => a -> a
sumTo' 0 = 0
sumTo' n = n + sumTo' (n - 1)

-- faster:
sumTo :: (Enum b, Num b) => Int -> b
sumTo n = foldl (+) 0 (take n [1..]) 

intToString :: Integral a => a -> String
intToString n = go (map digitToStr (digits n)) ""
    where go lst strg
            | null lst = strg
            | otherwise = go (tail lst) (strg ++ head lst)

digitToStr :: Integral a => a -> String
digitToStr n
    | n == 0 = "0"
    | n == 1 = "1"
    | n == 2 = "2"
    | n == 3 = "3"
    | n == 4 = "4"
    | n == 5 = "5"
    | n == 6 = "6"
    | n == 7 = "7"
    | n == 8 = "8"
    | n == 9 = "9"
    | otherwise = "error"


digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = "error"

-- changes an integer to a list of its digits f.e. 678 -> [6,7,8]
digits :: Integral a => a -> [a]
digits n = go n []
    where go num lst
            | num < 0 = [(-1)]
            | num == 0 = lst
            | otherwise = go (div num 10) ((mod num 10) : lst)

wordNumber :: Int -> String
wordNumber n = go (map digitToWord (digits n)) ""
    where go lst strg
            | null lst = strg
            | null (tail lst) = go (tail lst) (strg ++ head lst)
            | otherwise = go (tail lst) (strg ++ head lst ++ "-")

question1 :: [Char]
question2 :: [Char]
question3 :: [Char]
question1 = "\nReview of types"
question2 = "Reviewing currying\n" ++
            "Given the following definitions, tell us what value results from\n" ++
            "further applications.\n" ++
            "cattyConny :: String -> String -> String\n" ++
            "cattyConny x y = x ++ \" mrow \" ++ y\n" ++
            "flippy = flip cattyConny\n" ++
            "appedCatty = cattyConny \"woops\"\n" ++
            "frappe = flippy \"haha\""
question3 = "Recursion"

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "What is the type of [[True, False], [True, True], [False, True]]?\n" ++
                        "a) Bool\n" ++
                        "b) mostly True\n" ++
                        "c) [a]\n" ++
                        "d) [[Bool]]"
                 , question = question1
                 , result_lhs = "d) [[Bool]]"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "Which of the following has the same type as [[True, False],\n" ++
                        "[True, True], [False, True]]?\n" ++
                        "a) [(True, False), (True, True), (False, True)]\n" ++
                        "b) [[3 == 3], [6 > 5], [3 < 4]]\n" ++
                        "c) [3 == 3, 6 > 5, 3 < 4]\n" ++
                        "d) [\"Bool\", \"more Bool\", \"Booly Bool!\"]"
                 , question = question1
                 , result_lhs = "b) [[3 == 3], [6 > 5], [3 < 4]]"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "For the following function\n" ++
                        "func :: [a] -> [a] -> [a]\n" ++
                        "func x y = x ++ y\n" ++
                        "which of the following is true?\n" ++
                        "a) x and y must be of the same type\n" ++
                        "b) x and y must both be lists\n" ++
                        "c) if x is a String then y must be a String\n" ++
                        "d) all of the above"
                 , question = question1
                 , result_lhs = "d) all of the above"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "For the func code above, which is a valid application of\n" ++
                        "func to both of its arguments?\n" ++
                        "a) func \"Hello World\"\n" ++
                        "b) func \"Hello\" \"World\"\n" ++
                        "c) func [1, 2, 3] \"a, b, c\"\n" ++
                        "d) func [\"Hello\", \"World\"]"
                 , question = question1
                 , result_lhs = "b) func \"Hello\" \"World"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "What is the value of appedCatty \"woohoo!\" ?\n" ++
                        "a) \"haha mrow woohoo!\"\n" ++
                        "b) \"haha mrow woops!\"\n" ++
                        "c) \"woops mrow woohoo!\"\n" ++
                        "d) \"woops worm woohoo!\""
                 , question = question2
                 , result_lhs = "c) \"woops mrow woohoo!\""
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "frappe \"1\"\n" ++
                        "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "a) \"1 mrow haha\"\n" ++
                        "b) \"woops mrow haha\"\n" ++
                        "c) \"haha mrow 1\"\n" ++
                        "d) \"haha mrow woohoo\""
                 , question = question2
                 , result_lhs = "a) \"1 mrow haha\""
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "frappe (appedCatty \"2\")\n" ++
                        "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "a) \"haha mrow woops mrow 2\"\n" ++
                        "b) \"woops mrow 2 mrow haha\"\n" ++
                        "c) \"haha mrow 2\"\n" ++
                        "d) \"2 mrow woops mrow haha\""
                 , question = question2
                 , result_lhs = "b) \"woops mrow 2 mrow haha\""
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "cattyConny (frappe \"pink\") (cattyConny \"green\" (appedCatty \"blue\"))\n" ++
                        "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "a) \"pink mrow woohoo mrow woops mrow haha mrow blue\"\n" ++
                        "b) \"pink mrow green mrow haha mrow woops mrow blue\"\n" ++
                        "c) \"haha mrow pink mrow green mrow woohoo mrow blue\"\n" ++
                        "d) \"pink mrow haha mrow green mrow woops mrow blue\""
                 , question = question2
                 , result_lhs = "d) \"pink mrow haha mrow green mrow woops mrow blue\"" ++
                        "       False -> x\n" ++
                        "       True  -> y\n"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "cattyConny (flippy \"Pugs\" \"are\") \"awesome\"\n" ++
                        "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "a) \"are mrow Pugs mrow awesome\"\n" ++
                        "b) \"Pugs mrow are mrow awesome\"\n" ++
                        "c) \"awesome mrow are mrow Pugs\"\n" ++
                        "d) \"are mrow Pugs mrow awesome?\""
                 , question = question2
                 , result_lhs = "a) \"are mrow Pugs mrow awesome\n"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "a function that recursively sums all numbers from\n" ++
                        "1 to n, n being the argument. So that if n was 5, you’d add\n" ++
                        "1 + 2 + 3 + 4 + 5 to get 15. The type should be (Eq a, Num a)\n" ++
                        "=> a -> a.\n" ++
                        "a) sumTo' 0 = 0\n" ++
                        "   sumTo' n = sumTo' n + sumTo' (n - 1)\n" ++
                        "b) sumTo' 0 = 0\n" ++
                        "   sumTo' n = n + sumTo' (n + 1)\n" ++
                        "c) sumTo' 0 = 0\n" ++
                        "   sumTo' n = n - sumTo' (n - 1)\n" ++
                        "d) sumTo' 0 = 0\n" ++
                        "   sumTo' n = n + sumTo' (n - 1)\n"
                 , question = question3
                 , result_lhs = "d) sumTo' 0 = 0\n" ++
                        "   sumTo' n = n + sumTo' (n - 1)"
                 , yes_no = "d"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "a function that multiplies two integral numbers\n" ++
                        "using recursive summation. The type should be (Integral a)\n" ++
                        " => a -> a -> a\n" ++
                        "a) mulInt x 0 = 0\n" ++
                        "   mulInt x 1 = x\n" ++
                        "   mulInt x y = x + mulInt x (y - 1)\n" ++
                        "b) mulInt x 0 = 0\n" ++
                        "   mulInt x 1 = x\n" ++
                        "   mulInt x y = (y - 1) + mulInt x\n" ++
                        "c) mulInt x 0 = 0\n" ++
                        "   mulInt x 1 = x\n" ++
                        "   mulInt x y = x + flip mulInt (y - 1) x\n" ++
                        "d) mulInt x 0 = 0\n" ++
                        "   mulInt x 1 = y\n" ++
                        "   mulInt x y = x + mulInt x (y - 1)"
                 , question = question3
                 , result_lhs = "a) mulInt x 0 = 0\n" ++
                        "   mulInt x 1 = x\n" ++
                        "   mulInt x y = x + mulInt x (y - 1)\n" ++
                        "and this also works:\n" ++
                        "c) mulInt x 0 = 0\n" ++
                        "   mulInt x 1 = x\n" ++
                        "   mulInt x y = x + flip mulInt (y - 1) x"
                 , yes_no = "ac"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd'\n" ++
                        "Fixing dividedBy\n" ++
                        "dividedBy :: Integral a => a -> a -> (a, a)" ++
                        "dividedBy num denom = go num denom 0\n" ++
                        "    where go n d count\n" ++
                        "            | n < d = (count, n)\n" ++
                        "            | otherwise =\n" ++
                        "                go (n - d) d (count + 1)\n" ++
                        "Our dividedBy function wasn’t quite ideal. For one thing. It\n" ++
                        "was a partial function and doesn’t return a result (bottom)\n" ++
                        "when given a divisor that is 0 or less.\n" ++
                        "Work out the solution for your own - if you want to see a solution press a, b, c or d"
                 , question = question3
                 , result_lhs = "data DividedResult =\n" ++
                                "    Result Integer Integer\n" ++
                                "    | DividedByZero deriving Show\n\n" ++
                                "dividedBy :: Integer -> Integer -> DividedResult\n" ++
                                "dividedBy num 1 = Result num 0\n" ++
                                "dividedBy num 0 = DividedByZero\n" ++
                                "dividedBy 0 _ = Result 0 0\n" ++
                                "dividedBy num denom = go num denom 0\n" ++
                                "    where go n d count\n" ++
                                "            | n < d && n >= 0 && d > 0    = Result count n\n" ++
                                "            | n > 10000000000 * d && n >= 0 && d > 0 = go (n - d * 10000000000) d (count + 10000000000)\n" ++
                                "            | n > 100000 * d && n >= 0 && d > 0 = go (n - d * 100000) d (count + 100000)\n" ++
                                "            | n >= d && n >= 0 && d > 0    = go (n - d) d (count + 1)\n" ++
                                "            | n * (-1) < d  && n <= 0 && d > 0 = Result count n\n" ++
                                "            | n * (-1) >= d * 10000000000  && n <= 0 && d > 0 = go (n + d * 10000000000) d (count - 10000000000)\n" ++
                                "            | n * (-1) >= d * 100000  && n <= 0 && d > 0 = go (n + d * 100000) d (count - 100000)\n" ++
                                "            | n * (-1) >= d  && n <= 0 && d > 0 = go (n + d) d (count - 1)\n" ++
                                "            | n * (-1) < d * (-1) && n <= 0 && d < 0 = Result count n\n" ++
                                "            | n * (-1) >= d * (-1 * 10000000000) && n <= 0 && d < 0 = go (n - d * 10000000000) d (count + 10000000000)\n" ++
                                "            | n * (-1) >= d * (-1 * 100000) && n <= 0 && d < 0 = go (n - d * 100000) d (count + 100000)\n" ++
                                "            | n * (-1) >= d * (-1) && n <= 0 && d < 0 = go (n - d) d (count + 1)\n" ++
                                "            | n < d * (-1) && n >= 0 && d < 0 = Result count n\n" ++
                                "            | n >= d * (-1 * 10000000000) && n >= 0 && d < 0 = go (n + d * 10000000000) d (count - 10000000000)\n" ++
                                "            | n >= d * (-1 * 100000) && n >= 0 && d < 0 = go (n + d * 100000) d (count - 100000)\n" ++
                                "            | otherwise = go (n + d) d (count - 1)"
                 , yes_no = "abcd"
                 }
     ,Challenge { lhs = "Numbers into words\n" ++
                        "digitToWord :: Int -> String\n" ++
                        "digitToWord n = undefined\n" ++
                        "digits :: Int -> [Int]\n" ++
                        "digits n = undefined\n" ++
                        "wordNumber :: Int -> String\n" ++
                        "wordNumber n = undefined\n" ++
                        "Here undefined is a placeholder to show you where you need\n" ++
                        "to fill in the functions. The n to the right of the function names\n" ++
                        "is the argument which will be an integer.\n" ++
                        "Fill in the implementations of the functions above so that\n" ++
                        "wordNumber returns the English word version of the Int value like this:\n" ++
                        "Prelude> wordNumber 12324546\n" ++
                        "\"one-two-three-two-four-five-four-six\"\n" ++
                        "Work out the solution for your own - if you want to see a solution press a, b, c or d"
                 , question = question3
                 , result_lhs = "digitToWord :: Int -> String\n" ++
                                "digitToWord n\n" ++
                                "    | n == 0 = \"zero\"\n" ++
                                "    | n == 1 = \"one\"\n" ++
                                "    | n == 2 = \"two\"\n" ++
                                "    | n == 3 = \"three\"\n" ++
                                "    | n == 4 = \"four\"\n" ++
                                "    | n == 5 = \"five\"\n" ++
                                "    | n == 6 = \"six\"\n" ++
                                "    | n == 7 = \"seven\"\n" ++
                                "    | n == 8 = \"eight\"\n" ++
                                "    | n == 9 = \"nine\"\n" ++
                                "    | otherwise = \"error\"\n" ++
                                "digits :: Int -> [Int]\n" ++
                                "digits n = go n []\n" ++
                                "    where go num lst\n" ++
                                "            | num == 0 = lst\n" ++
                                "            | otherwise = go (div num 10) ((mod num 10) : lst)\n" ++
                                "wordNumber :: Int -> String\n" ++
                                "wordNumber n = go (map digitToWord (digits n)) \"\"\n" ++
                                "    where go lst strg\n" ++
                                "            | null lst = strg\n" ++
                                "            | null (tail lst) = go (tail lst) (strg ++ head lst)\n" ++
                                "            | otherwise = go (tail lst) (strg ++ head lst ++ \"-\")"
                 , yes_no = "abcd"
                 }
     ]

askChallenge :: Challenge -> String
askChallenge c = unlines [ question c
                         , ""
                         , lhs c
                         , ""
                         ]

type Answer = Char

verifyAnswer :: Challenge -> Answer -> Bool
verifyAnswer c a = (a `elem` yes_no c)

showResult :: Challenge -> Answer -> String
showResult c a = unlines [ result_lhs c  -- a:[]
                         , ""
                         , "Your answer is " ++ userResult
                         , ""
                         , "-----------------------------------"
                         , ""
                         ]
  where
    userResult = if verifyAnswer c a
                 then "correct, congratulations!"
                 else "sadly, completely and utterly wrong!"

execChallenge :: Challenge -> IO Bool
execChallenge c = do
  putStr $ askChallenge c
  answer <- getChar
  putStrLn ""
  putStrLn $ showResult c answer
  return $ verifyAnswer c answer

main :: IO ()
main = do
    rs <- mapM execChallenge xs
    let points = length . filter id $ rs
    let possiblePoints = length xs
    putStrLn $ "Congratulations, scored " ++ show points ++ "/" ++ show possiblePoints ++ " (" ++ 
        show (fromIntegral 100 * fromIntegral points/ fromIntegral possiblePoints) ++ "%)" ++ "!"
