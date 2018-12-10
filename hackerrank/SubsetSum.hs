module Main where
--import qualified Data.Text.IO as T
--import qualified Data.Text as T
import qualified Data.ByteString.Char8 as T
import Data.List (sortBy, sort, replicate)
import Control.Monad (forM_)
import Data.Vector.Unboxed (Vector, freeze)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Set as Set
type IntV = Vector Int
type TstV = Vector Int

-- get x lines
rdLn :: Int -> IO [Int]
rdLn 0 = return []
rdLn x = do
    cu_ <- T.getLine
    let cu = read $ T.unpack cu_
    cs <- rdLn (x - 1)
    return ([cu] ++ cs)

main :: IO ()
main = do
--    n_ <- T.getLine -- size of int list
--    let n = read $ T.unpack n_
--    cIL <- T.getLine
--    let intL = map (read . T.unpack) $ T.words cIL
----    let intL = replicate 100000 (1000000000 :: Int)
--    t_ <- T.getLine   -- number of testcases
--    let t = read $ T.unpack t_
--    tstL <- rdLn t

    all_ <- T.getContents
    let lines_ = T.lines all_
        n = read $ T.unpack $ head lines_
        rest = tail lines_
        intL = map (read . T.unpack) $ T.words (head rest)
        rest1 = tail rest
        t = read $ T.unpack $ head rest1
        tstL_ = tail rest1

--    tstL_ <- T.getContents
    let tstL = map (read . T.unpack) $ tstL_

--    putStrLn $ "size of int list: " ++ show n ++ " int list: " ++ show intL 
--        ++ "\ncount of test cases:" ++ show t ++ " test case list: " ++ show tstL
    let tstV = V.fromList srtdTstL  -- tstL
        -- sort tstL - remember sort order - on end sort back to orig. order
        (srtdTstL, srtOrd) = unzip $ sortBy srt (zip tstL [0..])
        srt (t,n) (t1,n1) = compare t t1
        seqInOrd :: [Int]
        seqInOrd = sortBy desc intL -- sort in reverse (descending)
        desc a b
          | a < b = GT
          | a > b = LT
          | a == b = EQ
        -- add helper item for summing
        seqInOrdV = V.fromList (0 : seqInOrd)
    --convert to mutable vector
    intMV <- V.unsafeThaw seqInOrdV
    -- create sums of current and previous value of descending sorted int list
    forM_ [0 .. n - 1] (\i -> do 
        oldSum <- M.read intMV i
        value <- M.read intMV (i + 1)
        M.write intMV (i + 1) (oldSum + value)
        )
    -- intMV now holding sums list
    -- convert to unmutable vector
    intV <- freeze intMV
--    error ("got data")
--    error (show $ V.length tstV)
    let find :: Int -> Int -> (Int, Int)
        find ti ptr = go ptr ptr lmt where   -- go ptr ptr lmt  -- go 1 1 lmt
            lmt = V.length intV - 1
            go li i lu    -- li lower limit, lu upper limit
                | tstV V.! ti > intV V.! i && i == lmt = ((-1), i)
                | i == lu = (i, i)
                | tstV V.! ti <= intV V.! i = go li divD i
                | tstV V.! ti > intV V.! i = go i divU lu
                | otherwise = (i, i)
                where
                divD = div (li + i + mod') 2
                mod' = mod (li + i) 2
                divU = div (i + lu + mod_) 2
                mod_ = mod (i + lu) 2

    -- create mutable vector for result
    resMV <- M.replicate (t + 1) (0 :: Int, 1 :: Int)
    forM_ [1 .. t] (\i -> do
        prev <- M.read resMV (i - 1)
        M.write resMV i (find (i - 1) (snd $ prev))
        )
    -- convert to unmutable vector
    resV_ <- freeze resMV
    -- eliminate first helper item
    let resV = V.tail resV_
    -- convert back to list
        resLp = V.toList resV
    -- get rid of pointers
        resL = map (\(x,p) -> x) resLp
        (bkSrtRslt, _) = unzip $ sortBy srtbk (zip resL srtOrd)
        srtbk (t,n) (t1,n1) = compare n n1
    --  sort back to original order in calling bkSrtRslt
    putStr $ unlines $ map show $ bkSrtRslt

{-
4
4 8 10 12
4
4
13
30
100
-}
