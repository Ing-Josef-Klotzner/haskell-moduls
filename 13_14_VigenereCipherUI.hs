-- 13_14_VigenereCipherUI.hs
module VigenereCipherUI where
import Data.Char
--import System.Exit (exitSuccess)

-- caesar right shift lower case (a-z)
cs_rt_lc :: Int -> Char -> Char
cs_rt_lc s x = chr ((rem (ord x + s - ord 'a') 26) + ord 'a')

-- caesar right shift upper case (A-Z)
cs_rt_uc :: Int -> Char -> Char
cs_rt_uc s x = chr ((rem (ord x + s - ord 'A') 26) + ord 'A')

-- caesar left shift lower case (a-z)
cs_lt_lc :: Int -> Char -> Char
cs_lt_lc s x = chr ((rem (ord x - s - ord 'z') 26) + ord 'z')

-- caesar left shift upper case (A-Z)
cs_lt_uc :: Int -> Char -> Char
cs_lt_uc s x = chr ((rem (ord x - s - ord 'Z') 26) + ord 'Z')

-- for uncaesar just negate s
caesar :: Int -> Char -> Char
caesar s x
    | s > 0 && isLower x = cs_rt_lc s x
    | s > 0 && isUpper x = cs_rt_uc s x
    | s < 0 && isLower x = cs_lt_lc (-s) x
    | s < 0 && isUpper x = cs_lt_uc (-s) x
    | otherwise = x

-- for those not understanding last comment:
uncaesar :: Int -> Char -> Char
uncaesar s x = caesar (-s) x

-- important! keyword is defined to be upper letter (like described in book)
key = "ALLY"
infKey :: String -> String
infKey key_ = key_ ++ infKey key_
teststring = "this is a text to be cyphered"
-- keypattern creates a string of repeating key, matching only characters (positions) to cipher
keypattern :: [Char] -> [Char] -> [Char]
keypattern "" strToCyph = keypattern "A" strToCyph -- leave it unchanged on empty keyword
keypattern key_ strToCyph = go "" 0 0
    where go kp scnt pcnt
            | length kp == length strToCyph = kp
            | not $ isLetter (strToCyph !! scnt) = go (kp ++ " ") (scnt + 1) pcnt
            | otherwise = go (kp ++ [(infKey key_) !! pcnt]) (scnt + 1) (pcnt + 1)
-- *VigenereCipher> keypattern teststring key
--"ALLY AL L YALL YA LL YALLYALL"

-- keyShiftList converts the keypattern into a list of integers, holding information of amount of shift
keyShiftList :: [Char] -> [Int]
keyShiftList keypat = [ord x - ord 'A' | x <- keypat] 
-- ' ' (Blanks) result in -33, but the number is irrelevant, because blanks 
-- and other whitespace characters are ignored in caesar cypher
-- *VigenereCipher Data.List> keyShiftList (keypattern teststring key)
--[0,11,11,24,-33,0,11,-33,11,-33,24,0,11,11,-33,24,0,-33,11,11,-33,24,0,11,11,24,0,11,11]

vigCyph kw xt = zipWith caesar (keyShiftList (keypattern kw xt)) xt
-- *VigenereCipher Data.List> vigCyph "ALLY" teststring
--"tstq id l reie ro mp ayascrpo"

unvigCyph kw xt = zipWith uncaesar (keyShiftList (keypattern kw xt)) xt
-- *VigenereCipher Data.List> unvigCyph "ALLY" "tstq id l reie ro mp ayascrpo"
--"this is a text to be cyphered"

main :: IO ()
main = do
    putStr "You want to cypher (c) or uncypher (u)? Type c or u and enter: "
    cypOrUncyp <- getLine
    if cypOrUncyp == "c" then
        putStr "Please enter text to cyper: "
    else
        putStr "Please enter text to uncyper: "
    textToCypher <- getLine
    putStr "For ceasar (c) or vigenere (v) cypher type c or v and enter: "
    kindOfCypher <- getLine
    if kindOfCypher == "c" then
        do  putStr "How much characters you want to shift? (1-26) "
            shift <- getLine
            if cypOrUncyp == "c" then
                putStrLn $ "Result of caesar cypher: " ++ map (caesar (read shift)) textToCypher
            else
                putStrLn $ "Result of uncaesar cypher: " ++ map (uncaesar (read shift)) textToCypher
    else
        do  putStr "Vigenere keyword: "
            keyWord <- getLine
            if cypOrUncyp == "c" then
                putStrLn $ "Result of vigenere cypher: " ++ vigCyph keyWord textToCypher
            else
                putStrLn $ "Result of unvigenere cypher: " ++ unvigCyph keyWord textToCypher
                
