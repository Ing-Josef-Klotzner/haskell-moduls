module Main where
import Control.Monad (forM)
import qualified Data.Vector as V


-- get x lines
rdLn :: Int -> IO [Int]
rdLn 0 = return []
rdLn x = do
    cu_ <- getLine
    let cu = read cu_
    cs <- rdLn (x - 1)
    return ([cu] ++ cs)

minim (y, x) = min y x

rotate mx r m n = undefined where
    yMax = m - 1; xMax = n - 1
--    mvFromTo (y, x) = case () of
--        _
--            | 

main :: IO ()
main = do
    cIL <- getLine
    let intL :: [Int]
        intL = map read $ words cIL
        m = head intL
        n = intL !! 1
        rotate = last intL
    mrL <- forM [1..m] (\_ -> do fmap (map (read :: String -> Int).words) getLine)
    let mrV = V.fromList (map V.fromList mrL)
    print mrV
    print $ (mrV V.! 0) V.! 0
