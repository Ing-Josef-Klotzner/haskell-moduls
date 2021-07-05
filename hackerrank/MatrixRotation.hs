{-# LANGUAGE FlexibleContexts #-}
module Main where
import qualified Data.ByteString.Char8 as T
import Control.Monad (Monad, forM, forM_)
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.Array.MArray as MA
import Data.Array.IO

--{-# INLINE loop #-}
--loop :: (Monad m) => Int -> (Int -> m ()) -> m ()
--loop bex f = go 0 where
--    go n    | n == bex  = return ()
--            | otherwise = f n >> go (n+1)

-- get x lines
rdLn :: Int -> IO [Int]
rdLn 0 = return []
rdLn x = do
    cu_ <- getLine
    let cu = read cu_
    cs <- rdLn (x - 1)
    return ([cu] ++ cs)

minim (y, x) = min y x

--rotate_ :: Show a => V.Vector (V.Vector a) -> Int -> Int -> Int -> String
rotate_ mx r m n = ringsV where
    yMax = m - 1; xMax = n - 1
    rgMax = (min m n) `div` 2 - 1
    v0 = V.empty
    -- create rVV vector of vectors (rings)
    ringsV = go 0 0 1 0 0 0 v0 v0 v0 v0 where
        go y x yd xd rgTly rgTlx rV rVV ctV ctVV
            | down || up = go (y + yd) x yd xd rgTly rgTlx add_mx_yx rVV addc_mx_yx ctVV
            | turnToRight = go y x 0 1 rgTly rgTlx rV rVV ctV ctVV
            | turnToLeft = go y x 0 (-1) rgTly rgTlx rV rVV ctV ctVV
            | right || left = go y (x + xd) yd xd rgTly rgTlx add_mx_yx rVV addc_mx_yx ctVV
            | turnToUp = go y x (-1) 0 rgTly rgTlx rV rVV ctV ctVV
            | turnToDownNR = go (y+1) (x+1) 1 0 (rgTly + 1) (rgTlx + 1) v0 (rVV `V.snoc` rV) v0 (ctVV `V.snoc` ctV)
            | otherwise = rot_rngs (rVV `V.snoc` rV) (ctVV `V.snoc` ctV)
            where
            addc_mx_yx = ctV `V.snoc` (y, x)
            add_mx_yx = rV `V.snoc` ((mx V.! y) V.! x)
            down = yd == 1 && xd == 0 && yMax - rgTly /= y
            turnToRight = not right && yMax - rgTly == y && rgTlx == x
            right = yd == 0 && xd == 1 && xMax - rgTlx /= x
            turnToUp = not up && xMax - rgTlx == x && yMax - rgTly == y
            up = yd == (-1) && xd == 0 && rgTly /= y
            turnToLeft = not left && rgTly == y && xMax - rgTlx == x
            left = yd == 0 && xd == (-1) && rgTlx /= x
            turnToDownNR = not down && rgTlx == x && rgTly == y && rgTly < rgMax
    -- ctVV_ count vector with orig coordinates (y, x)
    -- rVV_ vector organized in circles around orig vector; rVVr ... rotated
    -- mx orig vector (organized in lines / columns)
    rot_rngs rVV_ ctVV_ = go 0 v0 where
        go ring rVVr
            | ring <= rgMax = go (ring + 1) (rVVr `V.snoc` rVVrottd) 
            | otherwise = backToLines rVVr
            where
            len = V.length rVVrg
            rVVrottd = (V.slice (len - rot) rot rVVrg) V.++ (V.slice 0 (len - rot) rVVrg)
            rVVrg = rVV_ V.! ring
            rot = r `mod` (V.length (rVV_ V.! ring))
        backToLines :: V.Vector (V.Vector Int) -> IO ()
        backToLines rVVr = do
            xAm <- MA.newArray ((0, 0), (yMax, xMax)) 0
            let t = xAm :: IOArray (Int,Int) Int
            forM_ [0..rgMax] $ \ring -> do
                let len = (V.length $ rVVr V.! ring) - 1
                forM_ [0..len] $ \ptr -> do
                    let nr = (rVVr V.! ring) V.! ptr
                        (y, x) = (ctVV_ V.! ring) V.! ptr
                    MA.writeArray xAm (y, x) nr
            xA <- MA.freeze xAm
            let t1 = xA :: A.Array (Int, Int) Int

--        backToLines rVVr = go 0 0 mx where
--            go ring ptr oV
--                | ptr < len = go ring (ptr + 1) choV
--                | ptr == len && ring < rgMax = go (ring + 1) 0 oV
--                | otherwise = oV
--                where
--                len = V.length $ rVVr V.! ring
--                choV = oV V.// [(y, (oV V.! y) V.// [(x, nr)])]
--                nr = (rVVr V.! ring) V.! ptr
--                (y, x) = (ctVV_ V.! ring) V.! ptr 

--    out :: A.Array (Int,Int) Int -> String
            let out = go 0 [] where
                    go y s
                        | y == yMax = s ++ line
                        | otherwise = go (y + 1) sAdd
                        where
                        sAdd = s ++ (line ++ "\n")
                        line = intercalate " " liney
                        liney = go 0 [] where
                            go x l
                                | x == n = l
                                | otherwise = go (x + 1) (l ++ [show c])
                                where
                                c = xA A.! (y, x)
            putStrLn out

main :: IO ()
main = do
    cIL <- getLine
    let intL :: [Int]
        intL = map read $ words cIL
        m = head intL
        n = intL !! 1
        rotate = last intL
    mrL <- forM [1..m] (\_ -> do fmap (map (read :: String -> Int).words) getLine)
--    mr <- T.getContents
    let mrV = V.fromList (map V.fromList mrL)
--        mrL = map ((map (read :: String -> Int)) . words . T.unpack) $ T.lines mr

--    print mrV
    rotate_ mrV rotate m n

{- too slow:
250 289 42971434
3884938 27796162 90096973 55285954 67594404 9739006 39322053 71505144 ...

50513357 81697861 47876988 77017684 39913685 72469684 51731789 ...
-}
