-- 8_Fibonacci.hs
--module Fibonacci where
-- 1 1 2 3 5 8 13 ..
-- 1.618033988749895  ... Faktor between Fibonacci Numbers

fibonacci :: Integral a => Int -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci 2 = 2
fibonacci 3 = 3
fibonacci 4 = 5
fibonacci 5 = 8
fibonacci 6 = 13
fibonacci 7 = 21
fibonacci 8 = 34
fibonacci 9 = 55
-- for next 10 numbers the run time raises a 100 !! (calculating fibonacci 70 takes round about 1/2 year))
--fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- f-1 f-2 f-3 f-4 f-5 f-6
-- f = f-1 + f-2 = 2 * f-2 + f-3 = 3 * f-3 + 2 * f-4 = 5 * f-4 + 3 * f-5 = 8 * f-5 + 5 * f-6
-- f-1 = f-2 + f-3
-- f-2 = f-3 + f-4
-- f-3 = f-4 + f-5
-- f-4 = f-5 + f-6

fibonacci n =
    let
        n_9 = fibonacci (n - 9)
        n_10 = fibonacci (n - 10)
    in
        fibonacci 9 * n_9 + fibonacci 8 * n_10

-- needs    import Control.Parallel
--fibPar :: Int -> Int
--fibPar n =
--  let
--    n_3 = fibonacci (n-3)
--    n_2 = fibonacci (n-2)
--    n_4 = fibonacci (n-4)
--  in
--    (n_3 `par` n_2) `pseq` (n_2 + 2* n_3 + n_4)

fib :: Num a => Int -> (a, [a])
fib n = 
    let fib = 1: 1: zipWith (+) fib (tail fib)
    in (fib !! n, take (n + 1) fib)

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
--zipWith _ _ _ = []

--  inserted function exercise from chapter 10 - using scanl
fibs :: Num a => Int -> [a]
fibs x = take x fi 
    where 
        fi = 1: scanl (+) 1 fi

fibsN :: Num a => Int -> a
fibsN x = (fibs x) !! (x - 1)

-- also for chapter 10 - factorial function using scanl and (!!):
factorial :: (Enum a, Num a) => Int -> a
factorial 0 = 1
factorial n = (scanl (*) 1 [2..]) !! (n - 1)

-- direct formula
fibf :: (Floating a, Integral b) => b -> a
fibf n = 1/sqrt(5) * ( ( ( 1 + sqrt (5)) / 2 ) ^ n - ( ( 1 - sqrt (5)) / 2 ) ^ n )

main = do
print "Die wie vielte Fibonacci Zahl soll berechnet werden? (Zahl + Enter eingeben)"
zahl <- readLn
let fibo = fst $ fib (zahl - 1)
putStrLn $ show (zahl) ++ ". Fibonacci Zahl linear rekursiv berechnet ist " ++ show fibo
putStrLn $ show (zahl) ++ ". Fibonacci Zahl mit scanl berechnet ist " ++ show (fibsN zahl)
putStrLn $ "\nDie Zahl hat " ++ show (length $ show fibo) ++ " Stellen"
putStrLn $ show (zahl) ++". Fibonacci Zahl mit Formel berechnet " ++ " ist " ++ show (fibf zahl)
putStrLn $ "Wollen Sie auch die ganze Reihe bis zur " ++ show (zahl) ++ ". Zahl sehen? (j/n)"
reihe <- getChar
if reihe == 'j'
    then putStrLn $ "\nFibonacci Reihe bis zur " ++ show (zahl) ++ ". Zahl ist " ++ show (snd $ fib (zahl - 1)) ++
            "\nReihe mit Funktion mit scanl ist " ++ show (fibs zahl)
    else putStrLn ""
putStrLn $ show (zahl) ++". Fibonacci Zahl optimiert rekursiv berechnet " ++ " ist " ++ show (fibonacci (zahl - 1))
-- create list of factors between two Fibonacci Numbers   (1.618033988749895)
-- zipWith (/) (reverse $ snd $ fib 99) (reverse (snd $ fib 98))
