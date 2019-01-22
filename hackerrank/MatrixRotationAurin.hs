import Data.List
import Control.Monad

strip :: [a] -> [a]
strip = init . tail

unwrap :: [[a]] -> [[a]]
unwrap [] = []
unwrap mat = list : unwrap rest
    where 
        firstCol = [x | (x:_) <- tail mat]
        lastRow = tail . last $ mat
        lastCol = reverse [last row | row <- init mat]
        firstRow = reverse . init $ head mat
        list = firstCol ++ lastRow ++ lastCol ++ firstRow
        rest = strip $ fmap strip mat

unwrap2 :: [[a]] -> [[a]]
unwrap2 mat 
    | null mat = []
    | null (head mat) = []
    | otherwise = border : unwrap2 core
    where
        a = head $ head mat
        b = head $ last mat
        c = last $ last mat
        d = last $ head mat
        ab = strip $ [x | (x:_) <- mat]
        bc = strip $ last mat
        dc = strip $ [last row | row <- mat]
        cd = reverse dc
        ad = strip $ head mat
        da = reverse ad
        border = [a] ++ ab ++ [b] ++ bc ++ [c] ++ cd ++ [d] ++ da
        core = strip $ map strip mat


wrap :: Int -> Int -> [[a]] -> [[a]]
wrap 0 _ _ = []
wrap m 0 _ = replicate m []
wrap m n (border : inside) = res
    where
        (a:ab, rest)    = splitAt (m-1) border
        (b:bc, rest')   = splitAt (n-1) rest
        (c:cd, d:da)  = splitAt (m-1) rest'
        ad = reverse da
        dc = reverse cd
        aadd = (a : ad) ++ [d]
        bbcc = (b : bc) ++ [c]
        core = wrap (m-2) (n-2) inside
        block = zipWith (:) ab core
        middle = zipWith (\x y -> x ++ [y]) block dc
        res = (aadd : middle) ++ [bbcc]



rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate list k =  tail ++ head
    where 
        n = length list
        k' = (n-k) `mod` n
        (head, tail) = splitAt k' list


rotateMatrix :: Int -> Int -> [[a]] -> Int -> [[a]]
rotateMatrix m n mat k =
    wrap m n $ map (`rotate` k) $ unwrap2 mat
    
printMatrix :: [[Int]] -> IO ()
printMatrix mat = putStr $ unlines $ map (unwords . (map show)) mat    

main :: IO ()
main = do
    [m,n,k] <- readLst :: IO [Int]
    mat <- replicateM m readLst :: IO [[Int]]
    let res = rotateMatrix m n mat k    
    printMatrix res
    

readLst :: Read a => IO [a]
readLst = liftM (map read . words) getLine
