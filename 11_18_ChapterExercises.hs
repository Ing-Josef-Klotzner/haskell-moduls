-- 11_18_ChapterExercises.hs
module ChapterExercises_11_18 where
import Data.Char
import Data.List

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday
f Friday = "Miller Time"

--Use as-patterns in implementing the following functions:
--1. This should return True if (and only if) all the values in
--the first list appear in the second list, though they need
--not be contiguous.
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf s@(x:xs) (y:ys) =
    (x == y && isSubseqOf xs ys) || isSubseqOf s ys
--Prelude> isSubseqOf "blah" "wboloath"
--True

--Split a sentence into words, then tuple each word with the
--capitalized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words
        where
            go [] = []
            go (w@(c:cs):ws) = (w, toUpper c : cs) : go ws
--Prelude> capitalizeWords "hello world"
--[("hello", "Hello"), ("world", "World")]

--Language exercises
--1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord w@(c:cs) = if isUpper c then w else toUpper c : cs
--Example output.
--Prelude> capitalizeWord "Chortle"
--"Chortle"
--Prelude> capitalizeWord "chortle"
--"Chortle"

--Write a function that capitalizes sentences in a paragraph.
--Recognize when a new sentence has begun by checking
--for periods. Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph x = concat $ map ((++ "\n") . capitalizeLines ) $ lines x

capitalizeLines :: String -> String
capitalizeLines x = go (words x) True
        where go (w@(c:cs):ws) toCap
                | w == [] = []
                | toCap == True && ws /= [] && last w == '.' = (capitalizeWord w ++ " ") ++ go ws True
                | toCap == False && ws /= [] && last w == '.' = (w ++ " ") ++ go ws True
                | toCap == True && ws /= [] = (capitalizeWord w ++ " ") ++ go ws False
                | toCap == False  && ws /= [] = (w ++ " ") ++ go ws False
                | toCap == True = capitalizeWord w
                | otherwise = w
--Example result you should get from your function:
--Prelude> let s = "blah. woot ha."
--Prelude> capitalizeParagraph s
--"Blah. Woot ha."
testparagraph = "dies ist ein satz. das ist noch ein satz.\n" ++
                "das sollte eine neue Zeile sein.\n" ++
                "das auch."
-- *ChapterExercises_11_18 Data.List> putStrLn $ capitalizeParagraph testparagraph 
--Dies ist ein satz. Das ist noch ein satz.
--Das sollte eine neue Zeile sein.
--Das auch.


--Phone exercise

--Remember old-fashioned phone inputs for writing text
--where you had to press a button multiple times to get different
--letters to come up? You may still have to do this when you try
--to search for a movie to watch using your television remote
--control. You’re going to write code to translate sequences of
--button presses into strings and vice versa.
--So! Here is the layout of the phone:

-- ----------------------------------
-- |  1       |  2 ABC   |  3 DEF   |
-- ----------------------------------
-- |  4 GHI   |  5 JKL   |  6 MNO   |
-- ----------------------------------
-- |  7 PQRS  |  8 TUV   |  9 WXYZ  |
-- ----------------------------------
-- |  * ^     |  0 + _   |  # .,    |
-- ----------------------------------

--Where star (*) gives you capitalization of the letter you’re
--writing to your friends, and 0 is your space bar. To represent
--the digit itself, you press that digit once more than the letters it
--represents. If you press a button one more than is required to
--type the digit, it wraps around to the first letter. For example,
--2     -> 'A'
--22    -> 'B'
--222   -> 'C'
--2222  -> '2'
--22222 -> 'A'
--So on and so forth. We’re going to kick this around.
--1. Create a data structure that captures the phone layout
--above. The data structure should be able to express enough
--of how the layout works that you can use it to dictate the
--behavior of the functions in the following exercises.
---- fill in the rest.
data DaPhone = DaPhone
        {
        chars_ :: String        
        }

keyb :: [DaPhone]
keyb =  [DaPhone {chars_ = "1"},     DaPhone {chars_ = "2abc"}, DaPhone  {chars_ = "3def"},
         DaPhone {chars_ = "4ghi"},  DaPhone {chars_ = "5jkl"}, DaPhone  {chars_ = "6mno"},
         DaPhone {chars_ = "7pqrs"}, DaPhone {chars_ = "8tuv"}, DaPhone  {chars_ = "9wxyz"},
         DaPhone {chars_ = "*^"},    DaPhone {chars_ = "0+_"},  DaPhone  {chars_ = "#.,"}
        ]

--keyb :: DaPhone
--keyb =  DaPhone {"1"   , "2abc" , "3def" ,
--                "4ghi" , "5jkl" , "6mno" ,
--                "7pqrs", "8tuv" , "9wxyz",
--                "*^"   , "0+_"  , "#.,"}

--2. Convert the following conversations into the keypresses
--required to express them. We’re going to suggest types
--and functions to fill in order to accomplish the goal, but
--they’re not obligatory. If you want to do it differently, go
--right ahead.
convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

convert :: [String] -> [[(Digit, Int)]]
convert x = map (concat . map reverseTaps) x

-- *ChapterExercises_11_18 Data.List> convert convo
--[[('*',1),('9',2),('2',2),('6',3),('6',3),('2',2),('0',4),('7',2),('5',4),('2',2),('9',4),('0',4),('2',1),('0',1),('0',4),('7',3),('8',3),('3',3),('7',5),('8',2),('4',4),('6',4),('6',3),('7',5)],[('*',1),('9',4),('2',2)],[('*',1),('8',3),('0',4),('1',1),('7',5),('8',2),('0',4),('4',3),('2',2),('4',3),('2',2)],[('*',1),('5',4),('6',4),('5',4),('0',4),('6',4),('5',3),('#',2),('0',4),('*',1),('4',3),('2',2),('8',4),('3',3),('0',4),('8',3),('0',4),('3',3),('8',4),('3',3),('7',4),('0',4),('8',2),('2',2),('7',5),('8',2),('3',3),('3',2),('0',4),('2',2),('5',4),('2',4),('6',4),('4',3),('6',4),('5',4)],[('*',1),('5',4),('6',4),('5',4),('0',4),('9',4),('2',2)],[('*',1),('9',2),('6',4),('9',2),('0',4),('8',3),('7',4),('0',4),('2',4),('6',4),('6',4),('5',4),('0',4),('4',3),('2',2),('4',3),('2',2),('#',2),('0',4),('*',1),('8',3),('7',4),('0',4),('8',2),('8',3),('7',4),('6',3)],[('*',1),('6',4),('5',3),('#',2),('0',4),('*',1),('3',2),('6',4),('0',4),('8',3),('0',4),('8',2),('4',3),('4',4),('6',3),('5',3),('0',4),('*',1),('4',4),('0',4),('2',2),('6',2),('0',4),('7',2),('7',4),('3',3),('8',2),('8',2),('9',4),('0',4),('*',1),('5',4),('6',4),('5',4)],[('*',1),('5',4),('6',4),('5',4),('0',4),('9',4),('2',2)],[('*',1),('5',2),('8',3),('7',5),('8',2),('0',4),('6',2),('2',2),('5',3),('4',4),('6',3),('4',2),('0',4),('7',5),('8',3),('7',4),('3',3),('0',4),('7',4),('6',4),('3',4),('5',4),('0',4),('8',3),('7',4),('0',4),('8',2),('8',3),('7',4),('6',3)]]


-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int
-- returns related string of all possible letters behind a button, searched by one of them
chs :: Digit -> String
chs digit 
        | digit == ' ' = head $ filter (\y -> elem '_' y) $ map chars_ keyb
        | otherwise = head $ filter (\y -> elem digit y) $ map chars_ keyb
-- this version would allow to use different DaPhone Lists
chs_ :: [DaPhone] -> Digit -> String
chs_ keyb_ digit 
        | digit == ' ' = head $ filter (\y -> elem '_' y) $ map chars_ keyb_
        | otherwise = head $ filter (\y -> elem digit y) $ map chars_ keyb_
-- returns Digit and number of Presses of Char to present
-- count of presses = position of character (position starting with 1!)
reverseTaps :: Digit -> [(Digit, Int)]
reverseTaps d = go (chs $ toLower d) 1
    where go daphone prss
            | (daphone == [] || (toLower d) == head daphone) && 
                isUpper d = [('*', 1),(head $ chs $ toLower d, prss)]
            | daphone == [] || d == head daphone = [(head $ chs d, prss)]
            | otherwise = go (tail daphone) (prss + 1)

-- following is original suggestion (replaced by above solution)
reverseTaps'' :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps'' = undefined
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: String -> [(Digit, Presses)]
cellPhonesDead = concat . map reverseTaps
--3. How many times do digits need to be pressed for each
--message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = sum $ map snd x 
--4. What was the most popular letter for each message? What
--was its cost? You’ll want to combine reverseTaps and fingerTaps
--to figure out what it cost in taps. reverseTaps is a list be-
--cause you need to press a different button in order to get
--capitals.
-- *ChapterExercises_11_18 Data.List> fingerTaps $ cellPhonesDead "Wanna play 20 questions"
--71
mostPopularLetter :: String -> Char
mostPopularLetter x = snd $ head $ mostPopularLetters x
-- returns tuple with largest (count of occurance of letter, and letter)
mostPopularLetterL :: String -> (Int, Char)
mostPopularLetterL x = head $ mostPopularLetters x
-- sorted list of tuples with (count of occurance of letter, and letter)
mostPopularLetters :: String -> [(Int, Char)]
mostPopularLetters x = go (unique x) []
    where go uql popul
            | uql == [] = reverse $ sort $ popul
            | otherwise = go (tail uql) (popul ++ [(count (head uql) x, head uql)])
-- counts number of occurance of a characters in a string (or words in a list of string)
count :: (Eq a, Num a1) => a -> [a] -> a1
count x yl = go x yl 0
    where go x yl c
            | yl == [] = c
            | x == (head yl) = go x (tail yl) c + 1
            | otherwise = go x (tail yl) c
-- returns a string consisting of occuring characters (each only once) in input string
-- removing blank
unique :: String -> String
unique x = go x ""
    where go xl ulst
            | xl == [] = ulst
            | head xl `notElem` ulst && head xl /= ' ' = go (tail xl) (ulst ++ [(head xl)])
            | otherwise = go (tail xl) ulst

--5. What was the most popular letter overall? What was the
--most popular word?
coolestLtr :: [String] -> Char
coolestLtr x = mostPopularLetter (concat x)
-- *ChapterExercises_11_18> coolestLtr convo
--'a'
uniqueWords :: String -> String
uniqueWords x = go (words x) ""
    where go xl ulst
            | xl == [] = ulst
            | head xl `notElem` words ulst = go (tail xl) (ulst ++ " " ++ (head xl))
            | otherwise = go (tail xl) ulst
mostPopularWords :: String -> [(Int, String)]
mostPopularWords x = go (words (uniqueWords x)) []
    where go uql popul
            | uql == [] = reverse $ sort $ popul
            | otherwise = go (tail uql) (popul ++ [(count (head uql) (words x), head uql)])
coolestWord :: [String] -> String
coolestWord x = snd $ head $ mostPopularWords (concat x)
-- *ChapterExercises_11_18> coolestWord convo
--"ur"      ('u' has same count)

--Hutton’s Razor
--Hutton’s Razor is a very simple expression language that
--expresses integer literals and addition of values in that expres-
--sion language. The “trick” to it is that it’s recursive and the
--two expressions you’re summing together could be literals or
--themselves further addition operations. This sort of datatype
--is stereotypical of expression languages used to motivate ideas
--in research papers and functional pearls. Evaluating or folding
--a datatype is also in some sense what you’re doing most of the
--time while programming anyway.
--1. Your first task is to write the “eval” function which reduces
--an expression to a final sum.
data Expr = Lit Integer | Add Expr Expr
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add ex1 ex2) = eval ex1 + eval ex2
-- *ChapterExercises_11_18> eval (Add (Add (Lit 3) (Lit 4)) ( Add (Lit 5) (Add (Lit 6) (Lit 5)) ))
--23
--Example of expected output:
--Prelude> eval (Add (Lit 1) (Lit 9001))
--9002
--2. Write a printer for the expressions.
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add ex1 ex2) = printExpr ex1 ++ " + " ++ printExpr ex2
-- *ChapterExercises_11_18> printExpr (Add (Add (Lit 3) (Lit 4)) ( Add (Lit 5) (Add (Lit 6) (Lit 5)) ))
--"3 + 4 + 5 + 6 + 5"
--Expected output:
--Prelude> printExpr (Add (Lit 1) (Lit 9001))
--"1 + 9001"
--Prelude> let a1 = Add (Lit 9001) (Lit 1)
--Prelude> let a2 = Add a1 (Lit 20001)
--Prelude> let a3 = Add (Lit 1) a2
--Prelude> printExpr a3
--"1 + 9001 + 1 + 20001"


question1 = "\nGiven the following datatype:\n" ++
            "data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday"
question2 = ""

data Challenge = Challenge { lhs :: String
                           , question :: String
                           , result_lhs :: String
                           , yes_no :: String
                           }

type Collection = [Challenge]
xs :: Collection
xs = [ Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "we can say:\n" ++
                        "a) Weekday is a type with five data constructors\n" ++
                        "b) Weekday is a tree with five branches\n" ++
                        "c) Weekday is a product type\n" ++
                        "d) Weekday takes five arguments"
                 , question = question1
                 , result_lhs = "a) Weekday is a type with five data constructors"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "with the same datatype definition in mind, what is\n" ++
                        "the type of the following function, f?\n" ++
                        "f Friday = \"Miller Time\"\n" ++
                        "a) f :: [Char]\n" ++
                        "b) f :: String -> String\n" ++
                        "c) f :: Weekday -> String\n" ++
                        "d) f :: Day -> Beer"
                 , question = question1
                 , result_lhs = "c) f :: Weekday -> String"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "Types defined with the data keyword\n" ++
                        "a) must have at least one argument\n" ++
                        "b) must begin with a capital letter\n" ++
                        "c) must be polymorphic\n" ++
                        "d) cannot be imported from modules" 
                 , question = question2
                 , result_lhs = "b) must begin with a capital letter"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "The function g xs = xs !! (length xs - 1)\n" ++
                        "a) is recursive and may not terminate\n" ++
                        "b) delivers the head of xs\n" ++
                        "c) delivers the final element of xs\n" ++
                        "d) has the same type as xs"
                 , question = question2
                 , result_lhs = "c) delivers the final element of xs"
                 , yes_no = "c"
                 }


     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of Husky (10 :: Integer)?\n" ++
                        "a) Husky (10 :: Integer) :: Doggies a\n" ++
                        "b) Husky (10 :: Integer) :: Doggies Integer\n" ++
                        "c) Husky (10 :: Integer) :: Doggies Int\n" ++
                        "d) Husky (10 :: Integer) :: Doggies\n"
                 , question = question1
                 , result_lhs = "b) Husky (10 :: Integer) :: Doggies Integer"
                 , yes_no = "b"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of Mastiff \"Scooby Doo\"?\n" ++
                        "a) Mastiff \"Scooby Doo\" :: Doggies' [Char]\n" ++
                        "b) Mastiff \"Scooby Doo\" :: Doggies Char\n" ++
                        "c) Mastiff \"Scooby Doo\" :: Doggies [Char]\n" ++
                        "d) Mastiff \"Scooby Doo\" :: Scooby [Char]\n"
                 , question = question1
                 , result_lhs = "c) Mastiff \"Scooby Doo\" :: Doggies [Char]"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "Is DogueDeBordeaux a type constructor or a data constructor?\n" ++
                        "a) both\n" ++
                        "b) type constructor\n" ++
                        "c) data constructor\n" ++
                        "d) Mastiff \"Scooby Doo\" secret constructor\n"
                 , question = question2
                 , result_lhs = "a) both"
                 , yes_no = "a"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of DogueDeBordeaux?\n" ++
                        "a) DogueDeBordeaux :: doge -> DogueDeBordeaux dog\n" ++
                        "b) DogueDeBordeaux :: doge -> DogueDeBordeaux dogue\n" ++
                        "c) DogueDeBordeaux :: doge -> DogueDeBordeaux doge\n" ++
                        "d) DogueDeBordeaux :: doge -> DogueDeBordeaux doo\n"
                 , question = question2
                 , result_lhs = "c) DogueDeBordeaux :: doge -> DogueDeBordeaux doge"
                 , yes_no = "c"
                 }
     ,Challenge { lhs = "Type 'a' or 'b' or 'c' or 'd' and enter\n" ++
                        "What is the type of DogueDeBordeaux \"doggie!\"\n" ++
                        "a) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux Scooby\n" ++
                        "b) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux [Char]\n" ++
                        "c) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux doge\n" ++
                        "d) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux [String]\n"
                 , question = question2
                 , result_lhs = "b) DogueDeBordeaux \"doggie!\" :: DogueDeBordeaux [Char]"
                 , yes_no = "b"
                 }
     ]
askChallenge :: Challenge -> String
askChallenge c = unlines [ question c
                         , ""
                         , lhs c
                         , ""
                         ]

type Answer = String

verifyAnswer :: Challenge -> Answer -> Bool
verifyAnswer c a = yes_no c == a

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
  answer <- getLine
  putStrLn ""
  putStrLn $ showResult c answer
  return $ verifyAnswer c answer

main = do
    rs <- mapM execChallenge xs
    let points = length . filter id $ rs
    let possiblePoints = length xs
    putStrLn $ "Congratulations, scored " ++ show points ++ "/" ++ show possiblePoints ++ " (" ++ 
        show (fromIntegral 100 * fromIntegral points/ fromIntegral possiblePoints) ++ "%)" ++ "!"
