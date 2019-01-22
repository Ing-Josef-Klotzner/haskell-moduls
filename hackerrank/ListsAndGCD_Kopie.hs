module Main where
import Data.List (repeat)
import Control.Monad (replicateM, liftM, join)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as T
type VVec = V.Vector (V.Vector Int)
type PMap = M.Map Int Int

readLst :: IO [Int]
readLst = liftM (map read . words) getLine

main :: IO ()
main = do
    tc <- readLn :: IO (Int)
--    cont <- T.getContents
    listL <- replicateM tc readLst
    let listV = V.fromList $ map V.fromList listL
--        listL = map ((map (read :: String -> Int)) . words . T.unpack) $ T.lines cont
    -- to 2 dimensional vector for O(1) access
        gcd :: String
        gcd = go ptrM "" 1 where
            ptrM = M.fromList (zip [0 .. length listL - 1] (repeat 0))
            ptrV = V.map mapF listV
            mapF x = 0
            go :: PMap -> String -> Int -> String
            go ptrM_ res vNr
                | vlen == 0 = ""
                | vlen == 1 = unwords $ map show $ V.toList $ (listV V.! 0)
                | vNr == vlen = go ptrM_ res 1
                | ptrM_ M.! vNr > V.length (listV V.! vNr) - 1 = res
                | ptrM_ M.! 0 > V.length (listV V.! 0) - 1 = res
                | isSmaller = go (mInsAt vNr) res vNr
                | isBigger = go (mInsAt 0) res vNr
                | allHavePrime = go (mInsAt 0) (res ++ nPrime) vNr
                | otherwise = go ptrM_ res (vNr + 1)
                where
                mInsAt x = M.insert x (ptrM_ M.! x + 2) ptrM_
                isSmaller = prime vNr < fstPrime
                isBigger = prime vNr > fstPrime
                nPrime = show fstPrime ++ " " ++ show (minimum primeCntL) ++ " "
                primeCntL = map pcmapF [0 .. vlen - 1] 
                pcmapF x = (listV V.! x) V.! ((ptr x) + 1)
                allHavePrime = all (== fstPrime) othersPrimeL
                othersPrimeL = map prime [1 .. vlen - 1]
                prime x = (listV V.! x) V.! (ptr x)
                ptr x = ptrM_ M.! x
                fstPrime = (listV V.! 0) V.! (ptrM_ M.! 0)
                vlen = V.length listV
    putStrLn gcd    
