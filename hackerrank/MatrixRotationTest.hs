module Main where
import Control.Monad (forM_)
import qualified Data.Array as A
import qualified Data.Array.MArray as MA
import Data.Array.IO

main :: IO ()
main = do
    xMm <- MA.newArray ((0, 0), (2, 3)) 0
    let t = xMm :: IOArray (Int,Int) Int 

    -- just change element (1,1) to 1
    forM_ [0..1] $ \y -> do
        forM_ [0..1] $ \x -> do
            MA.writeArray xMm (y, x) 1

    xM <- MA.freeze xMm
    let a = xM :: A.Array (Int, Int) Int
    print xM

--main :: IO ()
--main = do
--    print x
