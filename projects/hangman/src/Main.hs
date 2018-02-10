module Main where
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
--import System.IO    --for openFile, hSetEncoding

-- created by Ing. Josef Klotzner

data Puzzle = Puzzle String [Maybe Char] [Char] Int
--                    [1]        [2]       [3]   [4]
--1. the word we’re trying to guess
--2. the characters we’ve filled in so far
--3. the letters we’ve guessed so far
--4. count of incorrect guesses (added by Josef, as this is normal Hangman behaviour)

instance Show Puzzle where
    show (Puzzle _ discovered guessed inc_c) =
       (intersperse ' ' $
        fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed ++
        " Incorrect guesses: " ++ show(inc_c)

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] 0

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c
-- *Main> fmap renderPuzzleChar [Nothing, Just 'h', Nothing, Just 'e', Nothing]
-- "_h_e_"

--type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)
allWords :: IO WordList
allWords = do
--    h <- openFile "data/dict.txt" ReadMode
--    hSetEncoding h latin1
--    dict <- hGetContents h
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
        where gameLength w =
                let l = length (w :: String)
                in      l >= minWordLength
                     && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, (length wl - 1))
    --      fill this part in ^^^
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

fillInCharacter :: Puzzle -> Char -> Bool -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s inc_count) c inc_Bool =
--                      [1]                  [2]
    Puzzle word newFilledInSoFar (c : s) inc_count_new where
--              [3]
        zipper guessed wordChar guessChar =
--          [4]     [5]     [6]      [7]
            if wordChar == guessed
            then Just wordChar
            else guessChar
--          [8]
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
--          [9]                                   [10]
        inc_count_new
            | inc_Bool == True = inc_count + 1
            | inc_Bool == False = inc_count
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

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = if elem c s then True else False 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = if elem c g then True else False

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
            return (fillInCharacter puzzle guess False)
        (False, _) -> do
            putStrLn "This character wasn't in\
                    \ the word, try another."
            return (fillInCharacter puzzle guess True)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed inc_c) =
    if inc_c > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then
        do putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
