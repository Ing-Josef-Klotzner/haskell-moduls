module Main where
import Control.Monad
import Control.Monad.State (State, get, gets, put, modify, evalState)
import Data.Map (Map)
import Data.List (elemIndex)
import qualified Data.Map as M

-- basic version plus looking 2 steps ahead -- even closer to correct result, but still wrong
tmAcc [] = 0
tmAcc acc
    | length acc < 2 = 1
    | length acc == 2 = 2
tmAcc acc = go 2 (sKP 0) (sKP 1) 2 where
    go cP lKP rKP time
        | length acc == cP = time
    go cP lKP rKP time
        | (clc + cnblc) < (crc + cnbrc) = 
--        | clc < crc = 
            go (cP + 1) cKP rKP (time + clc + 1)
        | otherwise = 
            go (cP + 1) lKP cKP (time + crc + 1)
        where
            cnblc
                | (cP + 2) == la =
                    if cD rKP ncKP < cD cKP ncKP
                    then cD rKP ncKP
                    else cD cKP ncKP
                | (cP + 2) < la =
                    if (cD rKP ncKP + if cD ncKP nncKP < cD lKP nncKP
                                            then cD ncKP nncKP
                                            else cD lKP nncKP) 
                    < 
                    (cD cKP ncKP + if cD ncKP nncKP < cD rKP nncKP
                                        then cD ncKP nncKP
                                        else cD rKP nncKP)
                    then cD rKP ncKP
                    else cD cKP ncKP
                | otherwise = 0
            cnbrc
                | (cP + 2) == la =
                    if cD lKP ncKP < cD cKP ncKP
                    then cD lKP ncKP
                    else cD cKP ncKP
                | (cP + 2) < la =
                    if (cD lKP ncKP + if cD ncKP nncKP < cD rKP nncKP
                                            then cD ncKP nncKP
                                            else cD rKP nncKP) 
                    < 
                    (cD cKP ncKP + if cD ncKP nncKP < cD lKP nncKP
                                        then cD ncKP nncKP
                                        else cD lKP nncKP)
                    then cD lKP ncKP
                    else cD cKP ncKP
                | otherwise = 0
            clc = cD lKP cKP
            crc = cD rKP cKP
            nncKP = sKP (cP + 2)
            ncKP = sKP (cP + 1)
            cKP = sKP cP
    la = length acc
    -- set keyboardpointer
    sKP ptr 
        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
        | otherwise = 9
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- basic version   plus looking 1 step ahead   ---   little better, but still wrong
tmAcc' [] = 0
tmAcc' acc
    | length acc < 2 = 1
    | length acc == 2 = 2
tmAcc' acc = go 2 (sKP 0) (sKP 1) 2 where
    go cP lKP rKP time
        | length acc == cP = time
    go cP lKP rKP time
        | (clc + cnblc) < (crc + cnbrc) = 
--        | clc < crc = 
            go (cP + 1) cKP rKP (time + clc + 1)
        | otherwise = 
            go (cP + 1) lKP cKP (time + crc + 1)
        where
            cnblc
                | (cP + 1) < la =
                    if cD rKP ncKP < cD cKP ncKP
                    then cD rKP ncKP
                    else cD cKP ncKP
                | otherwise = 0
            cnbrc
                | (cP + 1) < la =
                    if cD lKP ncKP < cD cKP ncKP
                    then cD lKP ncKP
                    else cD cKP ncKP
                | otherwise = 0
            clc = cD lKP cKP
            crc = cD rKP cKP
            ncKP = sKP (cP + 1)
            cKP = sKP cP
    la = length acc
    -- set keyboardpointer
    sKP ptr 
        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
        | otherwise = 9
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- basic version  ... taking next best step is not overall next best step!
tmAcc'' [] = 0
tmAcc'' acc
    | length acc < 2 = 1
    | length acc == 2 = 2
tmAcc'' acc = go 2 (sKP 0) (sKP 1) 2 where
    go cP lKP rKP time
        | length acc == cP = time
    go cP lKP rKP time
        | clc < crc = 
            go (cP + 1) cKP rKP (time + clc + 1)
        | otherwise = 
            go (cP + 1) lKP cKP (time + crc + 1)
        where
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = sKP cP
    la = length acc
    -- set keyboardpointer
    sKP ptr 
        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
        | otherwise = 9
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- this solves problem, but is much tooooo slow   ---  n = 24 .. 21s  ... n = 10000 .. impossible
tmAcc''' :: [Int] -> Int
tmAcc''' [] = 0
tmAcc''' acc
    | length acc < 2 = 1
    | length acc == 2 = 2
tmAcc''' acc = go 2 (sKP 0) (sKP gFR) 2 where
    gFR = tmAFR $ take depth acc
        where depth = 10
    go cP lKP rKP time
        | length acc == cP = time
    go cP lKP rKP time
        | lr < rr = lr
        | otherwise = rr
        where
            lr = go (cP + 1) cKP rKP (time + clc + 1)
            rr = go (cP + 1) lKP cKP (time + crc + 1)
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = sKP cP
    la = length acc
    -- set keyboardpointer
    sKP ptr 
        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
        | otherwise = 9
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

tmAccM acc = evalState (tmAccM' acc) M.empty
-- working solution with memoization  ---  memoization not effective - half as slow as tmAcc'''
tmAccM' :: [Int] -> State (Map (Int, Int, Int, Int) Int ) Int
tmAccM' [] = return 0
tmAccM' acc
    | length acc < 2 = return 1
    | length acc == 2 = return 2
tmAccM' acc = go 2 (sKP 0) (sKP 1) 2 where
    go cP lKP rKP time
        | length acc == cP = return time
    go cP lKP rKP time
        | elr < err = lr
        | otherwise = rr
        where
            elr = evalState lr M.empty
            err = evalState rr M.empty
            lr = getOrUpdate (cP + 1, cKP, rKP, time) (go (cP + 1) cKP rKP (time + clc + 1))
            rr = getOrUpdate (cP + 1, lKP, cKP, time) (go (cP + 1) lKP cKP (time + crc + 1))
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = sKP cP
    la = length acc
    -- set keyboardpointer
    sKP ptr 
        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
        | otherwise = 9
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

getOrUpdate :: (Ord k) => k -> State (Map k v) v -> State (Map k v) v
getOrUpdate k ifEmptyState = do
    maybeVal <- gets (M.lookup k)
    case maybeVal of
        Just v -> return v
        Nothing -> do
            ifEmpty <- ifEmptyState
            modify (M.insert k ifEmpty)
            return ifEmpty

-- memoized version of tmAcc'''  --- working, but still too slow  -- n = 648 .. 137s
tmAcM :: [Int] -> Int
tmAcM [] = 0
tmAcM acc
    | la < 2 = 1
    | la == 2 = 2
    where
        la = length acc
tmAcM acc = memoizeM go (2, (sKP 0), (sKP gFR), 2) where
    gFR = tmAFR $ take depth acc
        where depth = 10
    go :: Monad m => ((Int, Int, Int, Int) -> m Int) -> (Int, Int, Int, Int) -> m Int
    go f (cP, lKP, rKP, time)
        | cP == la = return time
    go f (cP, lKP, rKP, time) = do
        lr <- mlr
        rr <- mrr
        if lr < rr then mlr else mrr
        where
            mlr = f ((cP + 1), cKP, rKP, (time + clc + 1))
            mrr = f ((cP + 1), lKP, cKP, (time + crc + 1))
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = sKP cP
    la = length acc
    -- set keyboardpointer
    sKP ptr 
        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
        | otherwise = 9
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- --------------------------------------------------------------------------------------------------
-- previous variant of memoized tmAcM, converted acc to list of keyboard positions
--used in tmAcR to check next part of account number (depth) to get next best step
--liSm = 1  means left is smaller
tmAcP :: [Int] -> Int -> Int -> (Int, Int)
tmAcP [] lKP rKP = (0, 0)
tmAcP kBPL lKP rKP = memoizeM go (0, lKP, rKP, 0, (-1)) where
    go :: Monad m => ((Int, Int, Int, Int, Int) -> m (Int, Int)) 
                    -> (Int, Int, Int, Int, Int) -> m (Int, Int)
    go f (cP, lKP, rKP, time, liSm)
        | cP == la = return (time, liSm)
    go f (cP, lKP, rKP, time, liSm) = do
        lr <- mlr
        rr <- mrr
        if fst lr < fst rr then mlr else mrr
        where
            mlr = f ((cP + 1), cKP, rKP, (time + clc + 1), lBr)
            mrr = f ((cP + 1), lKP, cKP, (time + crc + 1), rBr)
            lBr = if liSm == (-1) then 1 else liSm
            rBr = if liSm == (-1) then 0 else liSm
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- previous variant of tmAcc''', which uses tmAcP for search with depth d to decide next step
tmAcR acc = tmAcR' (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAcR' :: [Int] -> Int
tmAcR' [] = 0
tmAcR' kBPL
    | length kBPL < 2 = 1
    | length kBPL == 2 = 2
tmAcR' kBPL = go 1 (kBPL !! 0) (kBPL !! gFR) 1 where
    gFR = tmAFR $ take depth kBPL
        where depth = 10
    go cP lKP rKP time
        | length kBPL == cP = time
    go cP lKP rKP time
        | if lOK then left else lr < rr = lr
        | otherwise = rr
        where
            depth = 10
            lOK = la > (2 + depth)
            left = (snd $ tmAcP kBPLP lKP rKP) == 1
            kBPLP = drop cP $ take (cP + depth) kBPL
            lr = go (cP + 1) cKP rKP (time + clc + 1)
            rr = go (cP + 1) lKP cKP (time + crc + 1)
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1) 

-- previous variant of memoized tmAcM, with already converted account list
--used to get best first right finger position on keyboard (rKP)
tmAFR acc = tmAFR_ (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAFR_ kBPL = go 1 1 100000000 where
    go idx min_idx min_cost
        | idx < la = go (idx + 1) new_min_idx new_min_cost
        | otherwise = min_idx
        where 
            la = length kBPL
            -- use memoization !
            cost = tmAFR' kBPL idx
            -- find Index for right position with Minimum step cost
            new_min_cost = if new_min_c then cost else min_cost
            new_min_idx = if new_min_c then idx else min_idx
            new_min_c = cost < min_cost

-- return time for certain right finger position (idx)
tmAFR' :: [Int] -> Int -> Int
tmAFR' [] idx = 0
tmAFR' kBPL idx = memoizeM go (1, (kBPL !! 0), (kBPL !! idx), 1) where
    go :: Monad m => ((Int, Int, Int, Int) -> m Int) 
                    -> (Int, Int, Int, Int) -> m Int
    go f (cP, lKP, rKP, time)
        | cP == la = return time
    go f (cP, lKP, rKP, time) =
        if cP > idx
        then do
            lr <- mlr
            rr <- mrr
            if lr < rr then mlr else mrr
        else if cP == idx
            then f (cP + 1, lKP, cKP, time + 1)
            else f (cP + 1, cKP, rKP, time + clc + 1)
        where
            mlr = f (cP + 1, cKP, rKP, time + clc + 1)
            mrr = f (cP + 1, lKP, cKP, time + crc + 1)
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- test output of list with sums of steps of each right finger position
tmAFRL'' acc = map (tmAFRL' (cvtToKbL acc)) (drop 1 acc)
    where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1

-- variant of memoized tmAcM
--used to get best first right finger position on keyboard (rKP)
tmAFRL acc = tmAFRL_ (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAFRL_ kBPL = (unjust $ ix $ map (tmAFRL' kBPL) (drop 1 kBPL)) + 1
    where
    unjust (Just x) = x
    unjust Nothing = 0
    ix x = elemIndex (minimum x) x

-- return time for certain right finger position
tmAFRL' :: [Int] -> Int -> Int
tmAFRL' [] rKP = 0
tmAFRL' kBPL rKP = memoizeM go (0, (kBPL !! 0), rKP, 0) where
    go :: Monad m => ((Int, Int, Int, Int) -> m Int) 
                    -> (Int, Int, Int, Int) -> m Int
    go f (cP, lKP, rKP, time)
        | cP == la = return time
    go f (cP, lKP, rKP, time) = do
        lr <- mlr
        rr <- mrr
        if lr < rr then mlr else mrr
        where
            mlr = f ((cP + 1), cKP, rKP, (time + clc + 1))
            mrr = f ((cP + 1), lKP, cKP, (time + crc + 1))
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- 
type StateMap a b = State (Map a b) b

memoizeM :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (fn x) M.empty where
  g x = do
    y <- t fn x  
    m <- get
    put $ M.insert x y m
--    newM <- get
    return y
--    return $ trace ("Map now contains\n" ++ M.showTree newM) y
  fn x = get >>= \m -> maybe (g x) return (M.lookup x m)

main :: IO()
main = do
    n <- readLn :: IO Int
    scoreTemp <- getLine
    let account = map (read :: String -> Int) . words $ scoreTemp
    let result = tmAcR account
    putStrLn $ show result

--216 Stellen
--3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3
--535 s
-- ... 11 s with tmAcM, 5 times the time with double length (432)

--452
--3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3 3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3
--1117
-- ... 50 s with tmAcM

--678
--3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3 3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3 3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3
--1675
-- ... 137 s with tmAcM

--22
--1 9 4 2 5 3 6 4 7 5 8 6 9 0 1 9 4 2 5 3 6 4
--54
-- ... 13s with tmAccM, time doubles with every additional digit !!

--24
--1 9 4 2 5 3 6 4 7 5 8 6 9 0 1 9 4 2 5 3 6 4 7 5
--58
-- ... 52 s with tmAccM, time doubles with every additional digit !!


--24
--1 9 4 2 5 3 6 4 7 5 8 6 9 0 1 9 4 2 5 3 6 4 7 5
--58
-- .. 21 s with tmAcc''', time doubles with every additional digit !!

--16
--1 9 4 2 5 3 6 4 7 5 8 6 9 0 1 9
--38
-- ... 12 s with tmAcc''', 

--17
--1 9 4 2 5 3 6 4 7 5 8 6 9 0 1 9 4
--42
-- ... 45 s with tmAcc''', ~ time * 3 for every additional digit !!!

--1
--1 9 4 2 5 3 6 4 7 5 8 6 9 0 1 9 4 2
--45
-- .. 130 s with tmAcc''', ~ time * 3 for every additional digit !!!

--14
--1 9 4 2 5 3 6 4 7 5 8 6 9 0
--30

--There is a famous old bank in Bangalore. It has just started the process of filling its database with bank account numbers of its clients. In order to put one account number to the database, an employee has to insert it from a piece of paper to a computer using a standard keyboard (without using number pad found on the right hand side of most keyboards). The weird thing is that every employee assigned to this task can type in using only 2 index fingers (one left hand index finger and one right hand index finger). 

--Below is the sample representation of number keys present in the keyboard. 

--1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 0

--He can perform any one of the following steps:

--He can move any one of his fingers to adjacent keys.
--He can press the key just below any of his fingers. But he can press only one key at a time.
--Each of the above steps takes 1 second. So moving a finger from key 3 to key 5 takes 2s, moving a finger from key 7 to key 2 takes 5s, and moving a finger from key 0 to key 8 takes 2s (Key 0 is the rightmost key). Similarly, pressing a single key takes 1 second.

--Write a program that computes the minimal time needed to add account number of an employee to the database. Before the process, an employee can place his finger wherever he wants. All digits should be inserted in the given order.

--Note

--It is not necessary that left finger will always lie on the left side of right finger. They can also lie on the same key, and in opposite direction also.
--Input 
--In the first line, there is a number n denoting the length of the bank account number. 
--In the second line, there are n digits separated by a single space denoting the bank account number.

--Output 
--In one and only line, print the minimum time (in seconds) required to rewrite the bank account number according to the above rules.

--Constraints 
--1 ≤ n ≤ 104

--Input #00

--2
--1 2
--Output #00

--2
--Input #01

--3
--1 0 3
--Output #01

--5
--Explanations 

--Test Case #00: An employee can put his left finger on key 1 and his right finger on key 2 before the process, so the whole process takes 2 seconds. 

--Test Case #01: An employee can put his left finger on key 1 and his right finger on key 0 before the process. From that position, it takes 2 seconds to press first two keys. After that, he can move his left finger from key 1 to key 3, which takes 2 seconds and then press it which takes additional second. The whole process takes 5 seconds. Note that key 0 is the rightmost key.
