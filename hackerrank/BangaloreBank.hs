module Main where
import Control.Monad
import Control.Monad.State (State, get, gets, put, modify, evalState)
import Data.Map (Map)
import Data.List (elemIndex)
import qualified Data.Map as M
import qualified Data.Set as Set

unique :: Ord a => [a] -> [a] 
unique xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

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

-- basic version  ... taking next best step is not overall next best step!
-- plus idea to create solution path and go from back to front to optimize until nothing to optimize
-- to define: optimize ... basicalliy get lowest possible sum by looking always on part of problem in reverse way
tmAc2 :: [Int] -> ([(Int, String, Int)], Int)
tmAc2 acc = tmAcc'2 (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAcc'2 :: [Int] -> ([(Int, [Char], Int)], Int)
tmAcc'2 [] = ([],0)
tmAcc'2 kBPL
    | length kBPL < 2 = ([],1)
    | length kBPL == 2 = ([],2)
tmAcc'2 kBPL = go 1 (kBPL !! 0) (kBPL !! gFR) 1 [(kBPL !! 0, "l", kBPL !! 0)] where
    gFR = tmAFR $ take depth kBPL
        where depth = 50
    go cP lKP rKP time rL
        | la == cP = (rL, time)
    go cP lKP rKP time rL
        | gFR > cP = go (cP + 1) cKP rKP (time + clc + 1) (rL ++ [(lKP, "l", cKP)])
        | clc < crc = go (cP + 1) cKP rKP (time + clc + 1) (rL ++ [(lKP, "l", cKP)])
        | otherwise = go (cP + 1) lKP cKP (time + crc + 1) (rL ++ [(rKP, "r", cKP)])
        where
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

{--
*Main> tmAc2M [1, 9, 4, 2, 5, 3, 6, 4, 7, 5, 8, 6, 9, 0, 1, 9, 4, 2, 5, 3, 6, 4, 7, 5, 3, 6, 4, 7, 5, 8]
([(0,"l",0),(8,"r",8),(8,"r",3),(0,"l",1),(3,"r",4),(1,"l",2),(4,"r",5),(2,"l",3),(5,"r",6),(3,"l",4),(6,"r",7),(4,"l",5),(7,"r",8),(8,"r",9),(5,"l",0),(9,"r",8),(8,"r",3),(0,"l",1),(3,"r",4),(1,"l",2),(4,"r",5),(2,"l",3),(5,"r",6),(6,"r",4),(3,"l",2),(4,"r",5),(2,"l",3),(5,"r",6),(3,"l",4),(6,"r",7)],71)
*Main> tmAc2 [1, 9, 4, 2, 5, 3, 6, 4, 7, 5, 8, 6, 9, 0, 1, 9, 4, 2, 5, 3, 6, 4, 7, 5, 3, 6, 4, 7, 5, 8]
([(0,"l",0),(8,"r",8),(0,"l",3),(3,"l",1),(1,"l",4),(4,"l",2),(8,"r",5),(2,"l",3),(5,"r",6),(3,"l",4),(6,"r",7),(4,"l",5),(7,"r",8),(8,"r",9),(5,"l",0),(9,"r",8),(0,"l",3),(3,"l",1),(1,"l",4),(4,"l",2),(8,"r",5),(2,"l",3),(5,"r",6),(3,"l",4),(4,"l",2),(6,"r",5),(2,"l",3),(5,"r",6),(3,"l",4),(6,"r",7)],79)
--}

-- this solves problem, but is much tooooo slow   ---  n = 24 .. 21s  ... n = 10000 .. impossible
--    with outerSpace                                  n = 24 .. 2s  
tmAc3 acc = tmAcc''' (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAcc''' :: [Int] -> Int
tmAcc''' [] = 0
tmAcc''' kBPL 
    | length kBPL < 2 = 1
    | length kBPL == 2 = 2
tmAcc''' kBPL = go 1 (kBPL !! 0) (kBPL !! gFR) 1 where
    gFR = tmAFR $ take depth kBPL
        where depth = 50            -- das mit depth muss nach Möglichkeit raus, wenn schnell genug
    go cP lKP rKP time
        | la == cP = time
    go cP lKP rKP time
        | gFR > cP = lr     -- get to pointer to account number of right finger when starting
        | rKP > lKP && cKP <= lKP || rKP < lKP && cKP >= lKP = lr   -- outerSpace optimization
        | rKP > lKP && cKP >= rKP || rKP < lKP && cKP <= rKP = rr   -- outerSpace optimization
        | lr < rr = lr
        | otherwise = rr
        where
            lr = go (cP + 1) cKP rKP (time + clc + 1)
            rr = go (cP + 1) lKP cKP (time + crc + 1)
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

-- memoized version of tmAcc'''  --- working, but still too slow  -- n = 642 .. 137s
           --           reduced with outerSpace to                   n = 642 .. 5 s
tmAcM :: [Int] -> Int
tmAcM acc = tmAcM' (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAcM' :: [Int] -> Int
tmAcM' [] = 0
tmAcM' kBPL
    | la < 2 = 1
    | la == 2 = 2
    where
        la = length kBPL
tmAcM' kBPL = memoizeM go (1, (kBPL !! 0), (kBPL !! gFR), 1) where
    gFR = tmAFR $ take depth kBPL
        where depth = 50
    go :: Monad m => ((Int, Int, Int, Int) -> m Int) -> (Int, Int, Int, Int) -> m Int
    go f (cP, lKP, rKP, time)
        | cP == la = return time
    go f (cP, lKP, rKP, time) 
        | gFR > cP = mlr
        | rKP > lKP && cKP <= lKP || rKP < lKP && cKP >= lKP = mlr   -- outerSpace optimization
        | rKP > lKP && cKP >= rKP || rKP < lKP && cKP <= rKP = mrr   -- outerSpace optimization
        | otherwise = do
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

tmAc3M :: [Int] -> Int
tmAc3M acc = tmAcM''' (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAcM''' :: [Int] -> Int
tmAcM''' [] = 0
tmAcM''' kBPL
    | la < 2 = 1
    | la == 2 = 2
    where
        la = length kBPL
tmAcM''' kBPL = memoizeM go (1, (kBPL !! 0), (kBPL !! gFR), 1,14, True, False) where
    gFR = tmAFR $ take depth kBPL
        where depth = 50
    go :: Monad m => ((Int, Int, Int, Int, Int, Bool, Bool) -> m Int) 
                    -> (Int, Int, Int, Int, Int, Bool, Bool) -> m Int
    go f (cP, lKP, rKP, time, dep, mains, ctsDwn)
        | cP == la && not mains || not mains && dep <= 0 = return time
        | cP == la && mains = return time
    go f (cP, lKP, rKP, time, dep, mains, ctsDwn) 
        | gFR > cP = mlr
        | rKP > lKP && cKP <= lKP || rKP < lKP && cKP >= lKP = mlr   -- outerSpace optimization
        | rKP > lKP && cKP >= rKP || rKP < lKP && cKP <= rKP = mrr   -- outerSpace optimization
        | ctsDwn = do 
            lr <- mlr
            rr <- mrr
            if lr < rr 
            then if mains then mlrm else mlr 
            else if mains then mrrm else mrr
        | otherwise = do
            lr <- mlrp
            rr <- mrrp
            if lr < rr 
            then if mains then mlrm else mlrp
            else if mains then mrrm else mrrp
        where
            depth = 14
            mlrp = f (cP + 1, cKP, rKP, time + clc + 1, depth, False, True)
            mrrp = f (cP + 1, lKP, cKP, time + crc + 1, depth, False, True)
            mlr = f (cP + 1, cKP, rKP, time + clc + 1, dep - 1, False, True)
            mrr = f (cP + 1, lKP, cKP, time + crc + 1, dep - 1, False, True)
            mlrm = f (cP + 1, cKP, rKP, time + clc + 1, dep - 1, True, False)
            mrrm = f (cP + 1, lKP, cKP, time + crc + 1, dep - 1, True, False)
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
    -- calculate difference on keyboard
    cD p1 p2 = abs (p2 - p1)

tmAc2M :: [Int] -> ([(Int, String, Int)], Int)
tmAc2M acc = tmAcM'' (cvtToKbL acc) where
    cvtToKbL = map kBP
    kBP 0 = 9
    kBP n = n - 1
tmAcM'' :: [Int] -> ([(Int, String, Int)], Int)
tmAcM'' [] = ([], 0)
tmAcM'' kBPL
    | la < 2 = ([], 1)
    | la == 2 = ([], 2)
    where
        la = length kBPL

tmAcM'' kBPL = memoizeM go (1, (kBPL !! 0), (kBPL !! gFR), 1, [(kBPL !! 0, "l", kBPL !! 0)]) where
    gFR = tmAFR $ take depth kBPL
        where depth = 50
    go :: Monad m => ((Int, Int, Int, Int, [(Int, String, Int)]) -> m ([(Int, String, Int)], Int)) 
                    -> (Int, Int, Int, Int, [(Int, String, Int)]) -> m ([(Int, String, Int)], Int)
    go f (cP, lKP, rKP, time, rL)
        | cP == la = return (rL, time)
    go f (cP, lKP, rKP, time, rL) 
        | gFR > cP = mlr
        | rKP > lKP && cKP <= lKP || rKP < lKP && cKP >= lKP = mlr   -- outerSpace optimization
        | rKP > lKP && cKP >= rKP || rKP < lKP && cKP <= rKP = mrr   -- outerSpace optimization
        | otherwise = do
            lr <- mlr
            rr <- mrr
            if snd lr < snd rr then mlr else mrr
        where
            outerSpace = elem cKP [0, 9]
            mlr = f (cP + 1, cKP, rKP, time + clc + 1, rL ++ [(lKP, "l", cKP)])
            mrr = f (cP + 1, lKP, cKP, time + crc + 1, rL ++ [(rKP, "r", cKP)])
            clc = cD lKP cKP
            crc = cD rKP cKP
            cKP = kBPL !! cP
    la = length kBPL
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
    go f (cP, lKP, rKP, time, liSm)
        -- Optimierung
        | rKP > lKP && cKP >= rKP || rKP < lKP && cKP <= rKP = mrr   -- outerSpace optimization
        | rKP > lKP && cKP <= lKP || rKP < lKP && cKP >= lKP = mlr   -- outerSpace optimization
        | otherwise = do
            lr <- mlr
            rr <- mrr
            if lr < rr then mlr else mrr
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
        where depth = 50
--    gFR = tmAFR kBPL
    go cP lKP rKP time
        | la == cP = time
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
--tmAFR acc = tmAFR_ (cvtToKbL acc) where
--    cvtToKbL = map kBP
--    kBP 0 = 9
--    kBP n = n - 1
tmAFR kBPL = go 0 0 100000000 where
    ukBPL = unique kBPL
    pNB = filter (/= (kBPL !! 0)) ukBPL   -- possible next best
--    pNB = filter (`elem` notOuterSpaceL) btwn
--    notOuterSpaceL = [4,5,6,7]
    go idx min_idx min_cost
        | idx < lu = go (idx + 1) new_min_idx new_min_cost
        | otherwise = unjust $ elemIndex (pNB !! min_idx) kBPL
        where 
            unjust (Just x) = x
            unjust Nothing = 0
            lu = length pNB
            -- use memoization !
            cost = tmAFR' kBPL (unjust $ elemIndex (pNB !! idx) kBPL)
            -- find Index for right position with Minimum step cost
            new_min_cost = if new_min_c then cost else min_cost
            new_min_idx = if new_min_c then idx else min_idx
            new_min_c = cost < min_cost
{--
    1 2 3 4 5 6 7 8 9 0
outer | btw | outer            | Cursorposition
                       outer ist incl cursorposition
    Wenn nächste Ziffer in outer liegt, kann sich nur nächstgelegener Finger dorthin bewegen
Ist also nächste Position 5 - 0, dann kann sich nur rechter Finger dorthin bewegen
check for right outer space
rKP > lKP  &&  cKP >= rKP || rKP < lKP && cKP <= rKP = rr (right moves)
rKP > lKP  &&  cKP <= lKP || rKP < lKP && cKP >= lKP = lr (left moves)
... besser am Anfang linken Finger dediziert auf links setzen und rechten auf rechten, dann:
cKP >= rKP = rr
cKP <= lKP = lr
oder (dann egal wer links oder rechts (Roberts Lösung)
definiere outerSpace = if lKP < rKP then [0..lKP] ++ [rKP .. 9] else [0..rKP] ++ [lKP..9]
... if cKP elem outerSpace then if clc < crc then lr else rr
-- If new position is in outer space (see above) - the cursor that is closer
-- has to move - positions can not swap.
--

acc ........ account number
kBPL  ...  Keyboard Pointer List (Keyboard Pointer 
                                                     converted acc)
cKP ....... current Key Pointer
rKP ........ right (finger) Key Pointer
lKP ........ left (finger) Key Pointer
lr ............ left result
rr ............ right result
mlr ......... monadic left result
mrr ......... monadic right result
cP .......... current pointer (to kBPL)
tmAcM ... time Account number Memoized
tmAcP ... variant of tmAcM being able to calculate part of kBPL
                with certain start condition lKP and rKP
liSm ........ left is smaller
--}

-- return time for certain right finger position (idx)
tmAFR' :: [Int] -> Int -> Int
tmAFR' [] idx = 0
tmAFR' kBPL idx = memoizeM go (1, (kBPL !! 0), (kBPL !! idx), 1) where
    go :: Monad m => ((Int, Int, Int, Int) -> m Int) 
                    -> (Int, Int, Int, Int) -> m Int
    go f (cP, lKP, rKP, time)
        | cP == la = return time
    go f (cP, lKP, rKP, time)
        | cP > idx && rKP > lKP && cKP >= rKP || cP > idx && rKP < lKP && cKP <= rKP = mrr -- outerSpace opt
        | cP > idx && rKP > lKP && cKP <= lKP || cP > idx && rKP < lKP && cKP >= lKP = mlr -- outerSpace opt
        | cP > idx = do
            lr <- mlr
            rr <- mrr
            if lr < rr then mlr else mrr
        | cP == idx = 
            f (cP + 1, lKP, cKP, time + 1)
        | otherwise =
            f (cP + 1, cKP, rKP, time + clc + 1)
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
tmAFRL_ :: [Int] -> Int
tmAFRL_ kBPL = (unjust $ ix $ map (tmAFRL' kBPL) (drop 1 kBPL)) + 1
    where
    unjust (Just x) = x
    unjust Nothing = 0
    ix x = elemIndex (minimum x) x

-- pack call from above to function below to get first best right finger position on keyboard
-- no: better: just search with possible next best (0-9 without lKP) instead of searching again and again
-- same numbers with rest of whole list
-- rKP initially set to 2nd acc (kBPL) position, searching for better one
tmAFRLM :: [Int] -> Int
tmAFRLM kBPL = memoizeM go (0, (kBPL !! 0), (kBPL !! 1), 0) where
    unjust (Just x) = x
    unjust Nothing = 0
    ix x = elemIndex (minimum x) x
    ukBPL = unique kBPL
    pNB = filter (/= (kBPL !! 0)) ukBPL   -- possible next best
    -- get list of possible next best (filter (/= (kBPL !! 0)) $ unique kBPL)
    -- go with each pNB finding smallest time
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
--1113
-- ... 50 s with tmAcM
--     5 s with outerSpace improvement

--678
--3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3 3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3 3 3 7 4 3 1 2 0 1 3 4 8 0 1 9 3 6 8 7 4 7 6 4 2 9 3 7 2 9 9 1 7 6 8 8 5 6 1 6 3 7 2 9 3 5 9 3 3 6 5 9 9 4 4 3 8 2 4 8 1 2 4 1 5 4 3 0 1 9 3 2 4 1 2 8 4 0 2 8 7 6 3 9 2 4 0 9 6 2 4 1 5 6 4 6 7 5 1 0 2 6 2 2 7 8 7 1 2 0 8 4 0 1 4 9 0 5 6 9 8 6 6 0 1 4 4 2 4 9 3 6 3 1 4 0 3 7 5 6 2 3 4 6 2 0 5 7 4 6 4 1 3 2 4 9 4 8 9 9 3 2 3 8 4 2 9 8 1 9 2 5 0 9 7 2 6 2 6 4 5 7 5 5 5 8 2 6 8 8 5 6 6 5 3 6 1 5 0 6 1 5 8 6 8 3 7 2 3 9 3 0 5 3 4 2 3 3 3 7 4 3 1 2 0 1 3
--1671
-- ... 137 s with tmAcM
--      11 s with outerSpace improvement
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

--18
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

--tmAccM acc = evalState (tmAccM' acc) M.empty
---- working solution with memoization  ---  memoization not effective - half as slow as tmAcc'''
--tmAccM' :: [Int] -> State (Map (Int, Int, Int, Int) Int ) Int
--tmAccM' [] = return 0
--tmAccM' acc
--    | length acc < 2 = return 1
--    | length acc == 2 = return 2
--tmAccM' acc = go 2 (sKP 0) (sKP 1) 2 where
--    go cP lKP rKP time
--        | length acc == cP = return time
--    go cP lKP rKP time
--        | elr < err = lr
--        | otherwise = rr
--        where
--            elr = evalState lr M.empty
--            err = evalState rr M.empty
--            lr = getOrUpdate (cP + 1, cKP, rKP, time) (go (cP + 1) cKP rKP (time + clc + 1))
--            rr = getOrUpdate (cP + 1, lKP, cKP, time) (go (cP + 1) lKP cKP (time + crc + 1))
--            clc = cD lKP cKP
--            crc = cD rKP cKP
--            cKP = sKP cP
--    la = length acc
--    -- set keyboardpointer
--    sKP ptr 
--        | ptr < la = if acc !! ptr == 0 then 9 else acc !! ptr - 1
--        | otherwise = 9
--    -- calculate difference on keyboard
--    cD p1 p2 = abs (p2 - p1)

--getOrUpdate :: (Ord k) => k -> State (Map k v) v -> State (Map k v) v
--getOrUpdate k ifEmptyState = do
--    maybeVal <- gets (M.lookup k)
--    case maybeVal of
--        Just v -> return v
--        Nothing -> do
--            ifEmpty <- ifEmptyState
--            modify (M.insert k ifEmpty)
--            return ifEmpty

