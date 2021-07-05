module Main where
import Data.List (replicate)
import Control.Monad (replicateM, liftM, join)
import qualified Data.Vector as V
type VVec = V.Vector (V.Vector Int)
type VPtr = V.Vector Int

readLst :: IO [Int]
readLst = liftM (map read . words) getLine

main :: IO ()
main = do
    let l = []
    tc <- readLn
    listL <- replicateM tc readLst
    -- to 2 dimensional vector for O(1) access
    let listV = V.fromList $ map V.fromList listL
        gcd :: String
        gcd = go ptrV "" 1 where
            ptrV = V.map mapF listV
            mapF x = 0
            go :: VPtr -> String -> Int -> String
            go ptrV_ res vNr
                | vlen == 0 = ""
                | vlen == 1 = unwords $ map show $ V.toList $ (listV V.! 0)
                | vNr == vlen = go ptrV_ res 1
                | ptrV_ V.! vNr > V.length (listV V.! vNr) - 1 = res
                | ptrV_ V.! 0 > V.length (listV V.! 0) - 1 = res
                | isSmaller = go (updt vNr) res vNr
                | isBigger = go (updt 0) res vNr
                | allHavePrime = go (updt 0) (res ++ nPrime) vNr
                | otherwise = go ptrV_ res (vNr + 1)
                where
                updt x = ptrV_ V.// [(x, ptrV_ V.! x + 2)]
                isSmaller = prime vNr < fstPrime
                isBigger = prime vNr > fstPrime
                nPrime = show fstPrime ++ " " ++ show (minimum primeCntL) ++ " "
                primeCntL = map pcmapF [0 .. vlen - 1] 
                pcmapF x = (listV V.! x) V.! ((ptr x) + 1)
                allHavePrime = all (== fstPrime) othersPrimeL
                othersPrimeL = map prime [1 .. vlen - 1]
                prime x = (listV V.! x) V.! (ptr x)
                ptr x = ptrV_ V.! x
                fstPrime = (listV V.! 0) V.! (ptrV_ V.! 0)
                vlen = V.length listV
    putStrLn gcd    
