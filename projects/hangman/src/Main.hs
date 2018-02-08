module Main where
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

data Puzzle = Puzzle String [Maybe Char] [Char]
--                    [1]        [2]       [3]
--1. the word we’re trying to guess
--2. the characters we’ve filled in so far
--3. the letters we’ve guessed so far

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
       (intersperse ' ' $
        fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = if elem c s then True else False 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = if elem c g then True else False

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c
-- *Main> fmap renderPuzzleChar [Nothing, Just 'h', Nothing, Just 'e', Nothing]
-- "_h_e_"

type WordList = [String]
allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
        where gameLength w =
                let l = length (w :: String)
                in      l >= minWordLength
                    && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, (length wl - 1))
    --      fill this part in ^^^
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
--                      [1]                  [2]
    Puzzle word newFilledInSoFar (c : s) where
--              [3]
        zipper guessed wordChar guessChar =
--          [4]     [5]     [6]      [7]
            if wordChar == guessed
            then Just wordChar
            else guessChar
--          [8]
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
--          [9]                                   [10]
--1. The first argument is our Puzzle with its three arguments,
--    with s representing the list of characters already guessed.
--2. The c is our Char argument and is the character the player
--    guessed on this turn.
--3. Our result is the Puzzle with the filledInSoFar replaced by
--    newFilledInSoFar the c consed onto the front of the s list.
--4. zipper is a combining function for deciding how to handle
--    the character in the word, what’s been guessed already,
--    and the character that was just guessed. If the current
--    character in the word is equal to what the player guessed,
--    then we go ahead and return Just wordChar to fill in that
--    spot in the puzzle. Otherwise, we kick the guessChar back
--    out. We kick guessChar back out because it might either be
--    a previously correctly guessed character or a Nothing that
--    has not been guessed correctly this time nor in the past.
--5. guessed is the character they guessed.
--6. wordChar is the characters in the puzzle word — not the
--    ones they’ve guessed or not guessed, but the characters
--    in the word that they’re supposed to be guessing.
--7. guessChar is the list that keeps track of the characters the
--    player has guessed so far.
--8. This if-then-else expression checks to see if the guessed
--    character is one of the word characters. If it is, it wraps it
--    in a Just because our puzzle word is a list of Maybe values.
--9. newFilledInSoFar is the new state of the puzzle which uses
--    zipWith and the zipper combining function to fill in char-
--    acters in the puzzle. The zipper function is first applied to
--    the character the player just guessed because that doesn’t
--    change. Then it’s zipped across two lists. One list is word
--    which is the word the user is trying to guess. The second
--    list, filledInSoFar is the puzzle state we’re starting with of
--    type [Maybe Char]. That’s telling us which characters in
--    word have been guessed.
--10. Now we’re going to make our newFilledInSoFar by using
--    zipWith. You may remember this from the Lists chapter.
--    It’s going to zip the word with the filledInSoFar values while
--    applying the zipper function from just above it to the
--    values as it does.

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that\
                        \ character, pick \
                        \ something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the\
                    \ word, filling in the word\
                    \ accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in\
                    \ the word, try another."
            return (fillInCharacter puzzle guess)

main :: IO ()
main = do
  putStrLn "hello world"
