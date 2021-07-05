-- 13_14_VigenereCipherUITest.hs
module VigenereCipherUITest where
import VigenereCipherUI
import Test.QuickCheck
import Data.Char (toUpper)

charGen :: Gen Char
charGen = elements (['a'..'z'] ++ ['A'..'Z']) -- ++ ['ä'] ++ ['ö'] ++ ['ü'] ++ ['Ä'] ++ ['Ö'] ++ ['Ü'])
charGen'' :: Gen Char
charGen'' = elements ['A'..'Z']

strGen :: Gen String
strGen = listOf charGen
strGen' :: Gen String
strGen' = listOf charGen''

--prop_caesar :: Int -> String -> Bool
--prop_caesar offset text = map (uncaesar offset) (map (caesar offset) text) == text

prop_caesar' :: Int -> Property
prop_caesar' offset = forAll strGen (\x -> map (uncaesar offset) (map (caesar offset) x) == x)

kw = "ABBY"

newtype MyString = MyString String--{ unwrapMyString :: String }
    deriving Show

instance Arbitrary MyString where
    arbitrary = MyString <$> strGen

newtype MyString'' = MyString'' String--{ unwrapMyString :: String }
    deriving Show

instance Arbitrary MyString'' where
    arbitrary = MyString'' <$> strGen'

prop_vigenere :: Property
--prop_vigenere offset = forAll strGen (\x y -> unvigCyph (map toUpper y) (vigCyph (map toUpper y) x) == x)
prop_vigenere = forAll strGen (\x -> unvigCyph kw (vigCyph kw x) == x)

prop_vigenere'' :: MyString'' -> MyString -> Bool
prop_vigenere'' (MyString'' kw'') (MyString x) = unvigCyph kw'' (vigCyph kw'' x) == x

prop_vigenere''' :: Property
prop_vigenere''' = forAll strGen (\x y -> unvigCyph y (vigCyph y x) == x)

main :: IO ()
main = do
    putStrLn "Testing ceasar cypher and back for random string (printable letters only)"
--    quickCheck prop_caesar
    quickCheck prop_caesar'
    putStrLn "Testing vigenere cypher and back for random string and keyword 'ABBY'"
    quickCheck prop_vigenere
    putStrLn "Testing vigenere cypher and back for random string and random upper case keyword"
    quickCheck (prop_vigenere'') --(MyString'' "ABBY") (MyString "TestString"))
    putStrLn "Testing vigenere cypher and back for random string and random keyword"
    quickCheck (prop_vigenere''')

