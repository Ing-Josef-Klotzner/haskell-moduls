-- 9_12_Cipher.hs
module Cipher where
import Data.Char

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
