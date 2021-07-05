-- tests/tests.hs
module HangmanTests where
import Test.QuickCheck
import Data.Char (chr)
import Hangman
import Data.List (elemIndices)
import Test.Hspec

--prop_thereAndBackAgain :: Property
--prop_thereAndBackAgain = forAll charGen 
--    (\c -> ((charToMorse c) >>= morseToChar) == Just c)
--prop_morseAndBackAgain :: Property
--prop_morseAndBackAgain = forAll morseGen 
--    (\c -> ((morseToChar c) >>= charToMorse) == Just c)

puzzle :: Puzzle
puzzle = freshPuzzle "testword"
puzzle1 = fillInCharacter puzzle 'a' True
puzzle2 = fillInCharacter puzzle1 'b' True

corrGuessedSoFar :: Eq a => [Maybe a] -> a -> Int -> Bool
corrGuessedSoFar g x p = g !! p == Just x

guessInLists :: Puzzle -> Char -> Bool
guessInLists (Puzzle word guessedSoFar guessed cnt) x
    | x `elem` word = (foldr (&&) True (map (corrGuessedSoFar guessedSoFar x) eI)) 
                        && head guessed == x
                        && cnt == 1
    | otherwise = head guessed == x
                && cnt == 1
        where
        eI = elemIndices x word

prop_guessInLists :: Char -> Bool
prop_guessInLists x = guessInLists (fillInCharacter puzzle x True) x

charGen :: Gen Char  --                      ä        ö        u
charGen = elements $ map chr ([97..122] ++ [228] ++ [246] ++ [252])

prop_guessInLists' :: Property
prop_guessInLists' = forAll charGen 
    (\c -> guessInLists (fillInCharacter puzzle c True) c)

--fillInCharacter   ... if guessed == wordChar it puts guessed char to filledInSoFar list AND to begin
--                      of list of chars already guessed AND, otherwise unchanged

main :: IO ()
main = 
    do
    putStrLn "check function fillIncharacter, if guessed lists are filled correctly ... "
    quickCheck prop_guessInLists
    putStrLn "check function fillIncharacter, if guessed lists are filled correctly (just letters) ... "
    quickCheck prop_guessInLists'
    hspec $ do
        describe "handleGuess" $
           do it "returns same puzzle on repeat guess" $
                  do let p = freshPuzzle "abc"
                     let p'@(Puzzle _ g1 gl1 _) = fillInCharacter p 'a' False
                     p''@(Puzzle _ g gl _) <- handleGuess p' 'a'
                     g `shouldBe` g1
                     gl `shouldBe` gl1
              it "correct guess fills puzzle" $ do
                 let p = freshPuzzle "abc"
                 p'@(Puzzle _ fill (l:lis) _) <- handleGuess p 'a'
                 [Just 'a', Nothing, Nothing] `shouldBe` fill
                 l `shouldBe` 'a'
