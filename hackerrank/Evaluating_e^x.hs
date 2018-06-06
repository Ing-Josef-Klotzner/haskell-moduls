module Main where
import Control.Monad
fakt 0 = 1
fakt 1 = 1
fakt n = n * fakt (n - 1) -- n!

eX9 x = go 18 where  -- max precision for Double with 18 terms (9 initially)
    go 0 = 1
    go y = x^y/fromIntegral (fakt y) + go (y - 1)

main :: IO()
main = do
    n <- readLn :: IO Int

    forM_ [1..n] $ \n_itr -> do
        x <- readLn :: IO Double
        putStrLn $ show (eX9 x)

--sample input

--4
--20.0000
--5.0000
--0.5000
---0.5000
