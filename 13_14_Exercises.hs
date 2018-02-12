module Exercises_13_14 where
import System.Exit (exitSuccess)
import Control.Monad
import Data.Char

-- also working on sentences by taking out all non-letter characters
palindrome :: IO ()
palindrome = forever $ do
    putStr "Please enter some text to check if palindrome: "
--    line <- getLine
    line <- getLine
    let line1 :: String
        line1 = map toLower $ filter isLetter line
    putStrLn line1
    case (line1 == reverse line1) of
        True -> putStrLn "It's a palindrome!"
        False -> do putStrLn "Nope!"
                    exitSuccess

-- just for fun - a getLine function for single characters - avoiding return of more than one character
getLine' :: IO [Char]
getLine' = do
    lin <- getLine
    case lin of
        [c] -> return [c]
        _   -> do   putStrLn "Only single characters!"
                    getLine'

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = Valid | NameEmpty | AgeTooHigh | AgeTooLow |
        NameEmpty_and_AgeTooHigh | NameEmpty_and_AgeTooLow |
        PersonInvalidUnknown String deriving (Eq, Show)
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 && age < 1000 = Right $ Person name age
    | name == "" && not (age > 0) = Left NameEmpty_and_AgeTooLow
    | name == "" && not (age < 1000) = Left NameEmpty_and_AgeTooHigh
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | not (age < 1000) = Left AgeTooHigh
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++
                                                " Age was: " ++ show age

-- Your job is to write the following function without modifying the code above.
gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Data entry for Person Collection"
    putStr "Name: "
    name <- getLine
    putStr "Age: "
    age <- getLine
    putStrLn "mkPerson function in progress to create person1 ..."
    let person1 = mkPerson name (read age)
--    putStrLn $ show (Person "Josef" 38)
    putStrLn $ show person1
    putStrLn "personal way of presentation of given person: Person Name Age  Validity"
    putStrLn $ show (person person1) ++ "  " ++ show (personi person1)
    putStrLn $ "way of presentation haskell book asked for: Either 'Yay! Person Name Age'\n" ++
             "                                              or   'Error: KindOfError'"
    putStrLn $ content person1

--Since IO () is about the least informative type imaginable,
--we’ll tell what it should do.
--a) It should prompt the user for a name and age input.
--b) It should attempt to construct a Person value using
--    the name and age the user entered. You’ll need the
--    read function for Age because it’s an Integer rather
--    than a String.
--c) If it constructed a successful person, it should print
--    ”Yay! Successfully got a person:” followed by the Per-
--    son value.
--d) If it got an error value, report that an error occurred
--    and print the error.

content :: Either PersonInvalid Person -> [Char]
content ei = case isRight ei of
    True -> "Yay! Successfully got a person: " ++ show (person ei)
    False -> "An Error occured: " ++ show (personi ei)

person :: Either a Person -> Person
person (Right a) = a
person (Left a) = Person "invalid" 0

personi :: Either PersonInvalid b -> PersonInvalid
personi (Left a) = a
personi (Right a) = Valid

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False 

