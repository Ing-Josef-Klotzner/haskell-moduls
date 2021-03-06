{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ViewPatterns #-}
module Main where
import Data.List ((\\), sort, repeat, sortBy, elemIndices, minimum, notElem, all, elemIndex)
-- import qualified Data.Vector as V
import qualified Data.Set as Set   -- (insert, member, empty)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable (Foldable, foldr,concatMap,concat)
import Control.Arrow
import Data.Bits
import Data.Int
import qualified Data.Array as A
import qualified Data.Maybe as Maybe
-- import qualified ArrayExtensions as AE
import Prelude hiding ( minimum, concat, notElem, concatMap, all, mapM_, foldr)
-- import GHC.Exts
-- 2 coordinate variables y and x are brought to 1 Int8
-- upper 4 bits is y, lower 4 bits is x 
-- limits maximum coordinates to 16x16, while 6x6 would
-- be sufficient based on hackerranks preconditions
type BlkIdx = Int
type Coord = (Int, Int)
-- change from relative to abs shape coordinates by shifting fCoor bits
-- fCoor = y * n + x
type BlockAI = Int  -- absolute shape
type Block = (Coord, [Coord])
-- 0-3 x blk0, 4-7 y blk0 ... length blk - 1
-- at length blk * 8: last moved block index
-- at (length blk + 1) * 8: target y, target x
type BoxKey = [Coord]
--   16 bits to store maximum 4x4 Block
type Shape = Int
-- index 0 target block
-- last index is last moved blockindex; Arrangement bits:
-- 0-7 last block index in BlockArray, 8-11 x block 0, 12-15 y block 0, ...
type BMap = M.Map BoxKey (Box, Box)
type Box' = A.Array Int Coord
type Box = (Box', BlockAI)
type BSArray = A.Array Int Shape   -- block shape array

-- Vector addition.
add :: (Num t, Num t1) => (t, t1) -> (t, t1) -> (t, t1)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
sub :: (Num t, Num t1) => (t, t1) -> (t, t1) -> (t, t1)
sub (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

unique :: (Ord a) => [a] -> [a]
unique xs = go Set.empty xs where
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise        = x : go (Set.insert x s) xs
    go _ _                 = []

-- list of tuples where fst of all tuples is unique
uniqFstTup :: Ord a => [(a, t)] -> [(a, t)]
uniqFstTup xs = go Set.empty xs where
    go s (x@(fx,sx):xs)
        | fx `Set.member` s = go s xs
        | otherwise = x : go (Set.insert fx s) xs
    go _ _ = []

---- 152 s  -- with fromList / toList
--mergesort'merge :: (Ord a) => V.Vector a -> V.Vector a -> V.Vector a
--mergesort'merge v ys
--    | V.null v = ys
--mergesort'merge xs w
--    | V.null w = xs
--mergesort'merge x y
--    | (V.head x < V.head y) = (V.slice 0 1 x) V.++ mergesort'merge (V.slice 1 zx x) y
--    | otherwise = (V.slice 0 1 y) V.++ mergesort'merge x (V.slice 1 zy y) where
--    zx = V.length x - 1
--    zy = V.length y - 1
-- 
--mergesort'splitinhalf :: V.Vector a -> (V.Vector a, V.Vector a)
--mergesort'splitinhalf xs = (V.slice 0 n xs, V.slice n zxs xs) where
--        n = len `div` 2
--        zxs = len - n
--        len = V.length xs
---- 
--mergesort :: (Ord a) => V.Vector a -> V.Vector a
--mergesort xs 
--    | (V.length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
--    | otherwise = xs where
--    (ls, rs) = mergesort'splitinhalf xs

--mergeSort :: Ord a => [a] -> [a]
--mergeSort xsL = V.toList $ mergesort (V.fromList xsL)

-- 121 s
bubblesort'iter :: Ord a => [a] -> [a] -> Bool -> ([a], Bool)
bubblesort'iter (x:y:xs) sortedL sorted
    | x > y = bubblesort'iter (x:xs) (sortedL ++ [y]) False
    | otherwise = bubblesort'iter (y:xs) (sortedL ++ [x]) sorted
bubblesort'iter x sortedL sorted = ((sortedL ++ x), sorted)

bubblesort' :: Ord a => ([a], Bool) -> Int -> [a]
bubblesort' (xs,sorted) i 
    | sorted == True = xs -- error ("list is sorted " ++ show xs ++ " " ++ show i)
    | i == length xs = xs
    | otherwise = bubblesort' (bubblesort'iter xs [] True) (i + 1) where
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' (xs, False) 0

-- 112 s
qsort_ []	= []
qsort_ (x:xs) = qsort_ small ++ mid ++ qsort_ large
    where
    small = [y | y<-xs, y<x]
    mid   = [y | y<-xs, y==x] ++ [x]
    large = [y | y<-xs, y>x]

-- 113 s
qsort []	= []
qsort l@(x:xs) = qsort small ++ mid ++ qsort large
    where
    small = [y | y<-xs, y<x]
    mid   = [y | y<-l, y==x]
    large = [y | y<-xs, y>x]

-- 104 s
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- 93,5 s, but compiled a bit faster than standard sort
insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort (firEle:arrLeft) = insert firEle (insertSort arrLeft)
    where insert a [] = [a]
          insert a (b:c)
            | a < b = a : b : c
            | otherwise = b : insert a c

-- get x lines
getLn :: Int -> IO [String]
getLn 0 = return []
getLn x = do
    cu <- getLine
    cs <- getLn (x - 1)
    return ([cu] ++ cs)

nMax :: Integer
nMax = 6  -- maximum size of puzzle
xMax :: Integer
xMax = 3  -- maximum size of block - must be <= 4 !!

-- encode coordinates to shape
coords2Shape :: (Foldable f) => Int -> f Coord -> Shape
coords2Shape n = foldr (\(y,x) -> (`setBit` (n*y+x))) 0

lREP_ :: (Num a, Bits a) => Int -> Int -> a
lREP_ m n = go 0 m where 
    go mkl 0 = mkl
    go mkl m_ = go (mkl `setBit` ((m_ - 1) * n)) (m_ - 1)

rREP_ :: (Num a, Bits a) => Int -> Int -> a
rREP_ m n = go 0 m where
    go mkr 0 = mkr
    go mkr m_ = go (mkr `setBit` ((m_ - 1) * n + n - 1)) (m_ - 1)

tREP_ :: (Num a, Bits a) => Int -> a
tREP_ n = go 0 n where
    go mkt 0 = mkt
    go mkt n_ = go (mkt `setBit` (n_ - 1)) (n_ - 1)

bREP_ :: (Num a, Bits a) => Int -> Int -> a
bREP_ m n = go 0 n where
    go mkb 0 = mkb
    go mkb n_ = go (mkb `setBit` ((n_ - 1) + n * (m - 1))) (n_ - 1)

-- to show decimal in binary format
dec2bin :: (Integral a, Show a) => a -> [Char]
dec2bin x = concatMap show $ reverse $ decToBin' x
  where
    decToBin' 0 = [0]
    decToBin' y = let (a,b) = quotRem y 2 
                    in [b] ++ decToBin' a

-- absolute shape
shape2Coords' :: Int -> Int -> BlockAI -> [Coord]
shape2Coords' m n shape = foldr (\z y -> if testBit shape z 
                                then (divMod z n) : y
                                else y) [] [0..(m * n - 1)]

createBlockMap :: Ord a => [a] -> [[a]] -> M.Map a Block
createBlockMap blk pz = go blk 0 0 M.empty where
    lp = length pz
    wp = length (pz !! 0)
    go [] pl pc blMap = blMap
    go bl pl pc blMap 
        | pl == lp = go (tail bl) 0 0 blPMap
        | pc == wp = go bl (pl + 1) 0 blMap
        | isB = go bl pl (pc + 1) bleMap
        | otherwise = go bl pl (pc + 1) blMap
        where
        b = head bl
        l
            | M.member b blMap = snd $ blMap M.! b
            | otherwise = []
        isB = b == pz !! pl !! pc
        el = l ++ [(pl, pc)]
        bleMap = M.insert b ((0,0), el) blMap
        blockPos = (\(y, x) -> (minimum y, minimum x)) $ unzip l
        blPMap = M.insert b (blockPos, l) blMap

-- element 0 of blockarray holds target block
-- last element of blockPosarray (Box) holds last moved blockindex (default 0)
-- just positions while BlockShapeArray holds shapes
createBlockArrayF :: Ord a => Int -> [a] -> [[a]] -> (Box, BSArray)
createBlockArrayF n blkL pz =    
    let blM = createBlockMap blkL pz
        getBl x = blM M.! x 
        arrL = zip [0..] (map (createShape . getBl) blkL)
        createShape (pos, blAbsPosL) = coords2Shape n $ map (sub pos) blAbsPosL
        blockShapeArr = A.array (0, len - 1) arrL
        parrL = zip [0..] ((map (fst . getBl) blkL) ++ [(0,0)])
        blockPosArr = A.array (0, len) parrL
        len = length blkL
        --
        shpsL = map (createAbsShape . getBl) blkL
        createAbsShape (pos, blAbsPosL) = coords2Shape n blAbsPosL
        box_AbsShp = foldr fldF 0 shpsL
        fldF shape box_ = box_ .|. shape
    in ((blockPosArr, box_AbsShp), blockShapeArr)

-- Is this a winning position? target has index 0
--isWin :: (Eq a, Num i, A.Ix i) => a -> A.Array i (a, b) -> Bool
isWinF :: Coord -> Box -> Bool
isWinF goal (box, _) = goal == box A.! 0

dirs_ :: (Num t, Num t1, Num t2) => t -> [((t1, t2), t)]
dirs_ n = [((0, -1),-1), ((0, 1),1), ((-1, 0),-1 * n), ((1, 0),n)]

-- test running funtion x times to see runtime
goL :: (Eq a, Num a) => (a -> [t]) -> a -> [t]
goL f x = go [] x where
    go y 0 = y
    go y x = go (f x) (x - 1)
goN :: (Eq a, Num a, Num t) => (a -> t) -> a -> t
goN f x = go 0 x where
    go y 0 = y
    go y x = go (f x) (x - 1)

-- Go level by level (level = all puzzles reachable with 1 step) 
-- through all reachable puzzles from starting puzzle
findPuzzlesF :: Coord -> Coord -> [[Char]] -> (Box, BSArray) -> [Char]
findPuzzlesF rlc goal blkL (start_, blSAr) = 
        go (M.singleton (reduceF start_) (start_, root_)) [start_] 0 where
    go _ [] c = "sorry, no solution found after cycles: " ++ show c
    go visited pzs c -- = goM visited pzs [] (c + 1) -- = goST visited pzs []
        | any (isWinF goal) pzs = out visited pzs
--        | any (isWinF goal) ps = outAll visited pzs
        | otherwise = goM visited pzs [] 0 -- goST visited pzs []
        where
        cvt_bML vmap x = cvtToOut (blkStepsL vmap x)
        outAll vmap pzL = concatMap (cvt_bML vmap) [winPz pzL]
        out vmap pzL = cvtToOut (blkStepsL vmap (minimum $ [winPz pzL]))

        -- getting next level moves
        goM :: BMap -> [Box] -> [Box] -> Int -> [Char]
        goM visitedM [] allPz1MA c
            -- for testing each level and show map (current, parent) or just next puzzles
            | c < 99999 = go visitedM allPz1MA c
--            -- show puzzles of allPz1MA with their parents
            | otherwise = "\n" ++ show (length pz_parentL) ++ "\n" ++ 
--                    concatMap (\(p,pa) -> showPz p ++
--                    " block, parent:\n" ++ showPz pa) pz_parentL
                    concatMap (\(p,pa) -> showPz p ++ show (lblk p) ++
                    " block, parent block: " ++ show (lblk pa) ++ "\n" ++ showPz pa) pz_parentL
            where
            pz_parentL :: [(Box, Box)]
            pz_parentL = map createPPar allPz1MA
--            createPPar (blk, pz) = (pz, parent blk pz) where
            createPPar :: Box -> (Box, Box)
            createPPar pz = (pz, parent) where
                parent = snd $ visitedM M.! (reduceF pz)
            lblk (pz,_) = fst $ pz A.! maxPBd
        goM visitedM (pz : pzsMR) allPz1MA c 
                = goD visitedM geoBlkIL [pz] allPz1MA c
        -- Depth first search with each block, target last
            where
            goD vstd [] _ allPz1M c -- = goM vstd pzsMR allPz1M c
                | any (isWinF goal) allPz1M = out vstd allPz1M
--                | any (isWinF goal) allPz1M = outAll vstd allPz1M
                | otherwise = goM vstd pzsMR allPz1M (c + 1)
            goD vstd blkLD [] allPz1M c = goD vstd (tail blkLD) [pz] allPz1M c
            goD vstd blkLD pzsD allPz1M c  = goD visitedD blkLD pzs'' (allPz1M ++ pzs'') c
                where
                blk = head blkLD
                (visitedD, pzs'') = newPositionsD pz vstd pzsD
                currBlkRShape = blSAr A.! blk

                -- Try to move the blk in puzzle in direction dir.
                -- Return puzzles with possible moved block - can be empty list
                moveBlkF :: Box -> [Box]
                moveBlkF (boxPArr, absShp) = go (dirs_ n) where
                    -- build abs shape of all blocks except blk
                    othersBox = absShp .&. complement currBlkAShape
                    currBlkPos@(y,x) = boxPArr A.! blk
                    currBlkAShape = currBlkRShape `shiftL` (y * n + x)
--                    currBlkAShape = currBlkRShape * 2 ^ (y * n + x)
                    go [] = []
                    go dirs''
                        | check = ((boxPArr A.// [(blk,add currBlkPos dir')]), newAbsShp) : go (tail dirs'')
                        | otherwise = go (tail dirs'') where
                        (dir', dir) = head dirs''
                        check = newNotInOtherBlks && newNotOut
                        newAbsShp = othersBox .|. new
                        new = currBlkAShape `shift` dir
                        newNotInOtherBlks = new .&. othersBox == 0
                        chk pat = currBlkAShape .&. (pat m n) == 0
                        newNotOut = case dir' of
                            (0, -1) -> chk lREP_
                            (0, 1) -> chk rREP_
                            (-1, 0) -> currBlkAShape .&. (tREP_ n) == 0
                            (1, 0) -> chk bREP_
                -- Given a list of puzzles, return the list of different
                -- puzzles (with each parent in a tuple) that are reachable from them in exactly one move.
                allMovesF :: Box -> [(Box)] -> [(Box, Box)]
                allMovesF parent pzs = concatMap allMoves1 pzs where
                    allMoves1 pz = zip (moveBlkF pz) (repeat parent)
                addPositionF :: BMap -> (Box, Box) -> (BMap, Maybe Box)
                addPositionF visited (p, parent) = (visited', q)
                    where 
                    old_p_parent = M.lookup (reduceF p) visited
                    (visited', q) = case old_p_parent of
                        Nothing -> (M.insert (reduceF p) (p, parent) visited, Just p)
                        Just _ -> (visited, Nothing)
                addPositionsF :: BMap -> [(Box, Box)] -> (BMap, [Box])
                addPositionsF visited [] = (visited, [])
                addPositionsF visited (((p,pAb), pr_@(parent,_)) : ps) = (visited'', qs)
                    -- do not process again, if p == parent, but store as visited
                    where
                    qs = case q of
                        Just (p', p'Ab) -> case () of
                            _ | plb == parentlb -> ps'
                            _ | otherwise -> ((p' A.// [(maxPBd, (blk, 0))]), p'Ab) : ps'
                            where--                add last block
                            -- compare new / parent without last block
                            parentlb = parent A.// [(maxPBd, (0,0))]
                            plb = p' A.// [(maxPBd, (0,0))]
                        Nothing -> ps'
                    (visited', ps') = addPositionsF visited ps
                    (visited'', q) = addPositionF visited' (p'', pr_)
                    -- last block in box p packen
                    p'' = (p A.// [(maxPBd, (blk, 0))], pAb)
                -- Given the map of visited puzzles and the list
                -- of current puzzles, return an updated map with all next moves
                newPositionsD :: Box -> BMap -> [Box] -> (BMap, [Box])
                newPositionsD parent visited curr_pzs = 
                    addPositionsF visited (allMovesF parent curr_pzs)
        
    -- sorted by block shapes and then their positions, only positions used
    reduceF :: Box -> BoxKey
    reduceF (box, _) = (box A.! maxPBd) : (box A.! 0) : sortedPossL
        where
        sortedPossL = map snd $ sort $ zip blSl posL0box
        posL0box = tail $ A.elems box  -- posL0box without target

    maxBd = snd (A.bounds blSAr)
    maxPBd = snd (A.bounds start)
    start = fst start_
    blSL = A.elems blSAr
    blSl = tail blSL
    blkIL = A.indices blSAr
    -- current block blanked block list array - faster (vs filter)
    cBbblSLArr = A.array (0, 35) [(blk, (A.elems (blSAr A.// [(blk,0)]))) | blk <- [0..35]]
    (m, n) = rlc  -- box dimensions

    v = m - 1; h = n - 1  --vMax, hMax
    start'' = A.array (0, (snd $ A.bounds start) - 1) (init $ A.assocs start)
    geoBlkIL = geoSorIdxL start''  -- start without "last block moved"
    --assumption: best to sort by size of block ascending and then 
    --sort them in shortest geometrical distance (sortBy position)
    geoSrt p = sortBy (\(_,(y1,x1)) (_,(y2,x2)) 
        -> compare ((v-y1)^2+(h-x1)^2) ((v-y2)^2+(h-x2)^2)) p
    geoSorIdxL p = map (\(idx,(pos,posL)) -> idx) $ geoSrt (A.assocs p)
--    assocs_ x = zip [0..maxBd] (map (getBlkP x) [0..])
    winPz pzss = filter (isWinF goal) pzss
    -- map list of puzzles from start to winning puzzle
--    getPathL :: BMap -> [Box] -> [Box']
    getPathL vstd pzss = go [fst $ head pzss] where
        go pathL
            | parent == root = pathL 
            | otherwise = go (parent : pathL) where
            (_, (parent,_)) = vstd M.! (reduceF ((head pathL),0))
    -- create list of blocks and their single step moves by finding 
    -- the different blocks of neighboring puzzles of solution path
    
    blkStepsL :: BMap -> [Box] -> [([Char], Coord, Coord)]
    blkStepsL vstd pzss_ = map blkMove blkLNeighborsL where
        posAL = map A.assocs (getPathL vstd pzss_)
        blkLNeighborsL = zip posAL (tail posAL)
        blkMove (l1, l2) = head $ map cvtToBlkMvs $ filter diffItems $ zip l1 l2 where
            cvtToBlkMvs ((i, blkP), (i1, blkP1)) = ((blkL !! i), blkP, blkP1)
            diffItems (x, y) = x /= y
--    cvtToOut :: _
    cvtToOut blkmvL = show len ++ "\n" ++ (unlines $ map str blkmvL)
        where
        len = length blkmvL
        str (x,y,z) = x ++ " " ++ show y ++ " " ++ show z
    root = A.array (A.bounds start) [(blki,((-1::Int,0::Int))) | blki <- A.indices start]
    root_ = (root,0)
    showPz (pz,_) = (unlines $ map unwords $ map (map getBlkDgt) allPosL) ++ "\n"
        where
        allPosL = [[(y,x) | x <- [0..h]] |  y <- [0..v] ]
        blkIL = A.indices blSAr
        
        posL :: BlkIdx -> [Coord]
        posL blkidx = shape2Coords' m n blkAShape where
            blkAShape = blkRShape `shiftL` (y * n + x)
            (y,x) = pz A.! blkidx
            blkRShape = blSAr A.! blkidx
        getBlkDgt :: Coord -> [Char]
        getBlkDgt pos
            | idxL == [] = map (const '.') (head blkL)
            | otherwise = blkL !! (snd $ head idxL)
            where
            idxL = filter isPosInA $ zip (map posInA blkIL) [0..]
            isPosInA (x, _) = x == True
            posInA x = elem pos (posL x)

-- for move target to beginning of list, n is targetIndex
moveToHead :: Int -> [a] -> [a]
moveToHead n as = head ts : (hs ++ tail ts)
   where (hs, ts) = splitAt n as

main :: IO ()
main = do
    m_nL <- fmap (map (read :: String -> Int).words) getLine
    let m = m_nL !! 0   -- y (lines)
        n = m_nL !! 1   -- x (columns)
    p <- getLn m   -- puzzle (max 6 x 6)
        -- blocks f.e. ["A", "B", "C"] (max 3 x 3 each)
    targS <- getLine
    goalL <- fmap (map (read :: String -> Int).words) getLine
    let blo = unique $ concat $ map (words . filter (/= '.')) p
        Just targIdx = if mBIdx /= Nothing then mBIdx else Just 0  -- make 0 default, if target not existing
        mBIdx = elemIndex targS blo
        bl = moveToHead targIdx blo
        pz = map words p
        blM = createBlockMap bl pz
        blAF@((blA_, absShp), blSA) = createBlockArrayF n bl pz
--        blA_lesbar = zip [0..length bl] (map (getBlkP blA_) [0..])
        isOneBlk (i, x) = (i, (length $ snd x) == 1)
        goal = (goalL !! 0, goalL !! 1)
    putStr $ findPuzzlesF (m, n) goal bl blAF
--    putStr $ findPuzzles (m, n) goal bl blA
{-
    putStrLn $ show pz ++ "  Target: " ++ show targS ++ " Goal: " ++ show goal ++ "  (0,0): " ++ show ((p !! 0) !! 0)
        ++ "  Blocks: " ++ show bl ++ "  Targetindex: " ++ show targIdx ++ " changed to 0"
    putStrLn $ "BlockMap: " ++ show blM
    putStrLn $ "Block Array neu: " ++ show blA_
    putStrLn $ "Block Shape Array: " ++ show blSA
    putStrLn $ "reduced Block List: " ++ show (reduce blA)
    putStrLn $ show (allMoves1_ (m, n) blA)
    putStrLn $ "isWin: " ++ show (isWin goal blA)
    putStrLn $ "all moves from start: " ++ show (allMoves (m, n) [blA])
-}
{-
-- blkStepsL:
-- [("B",(1,1),(1,2)),("A",(0,0),(1,0)),("B",(1,2),(0,2)),("B",(0,2),(0,1)),("B",(0,1),(0,0))]
*Main> main
Input:
3 4
. A . .
A B . C
. . . C
B
0 0
Output:
B (1,1) (1,2)
A (0,0) (1,0)
B (1,2) (0,0)

3 5
bs bs tw tw dw
bs ts ts rw ds
kw ks lw .. ..
bs
0 3

25

3 6
A . . . C .
A . B . C .
D . B . . .
D
0 5
Output:
D (2,0) (0,5)

testcase 12:
5 4
A B B C
A B B C
D E E F
G H H I
. J J .
B
3 1
(sollten 102 sein)  "B","A","C","D","E","F","G","H","I","J"
2:18
1:52 after optimizing reduce to just [Coord]

testcase 8:   5:32  ->  1:58 (eliminating p == parent in backstream, while mark as visited)
5 4                           and optimize reduce to just sorted positions (blockshape then position)
RW BS BS RS
RW BS BS RS
TW TS LW LS
BW DW DS KW
KS .. .. BB
BS
3 1

A B B C
A B B C
1 2 3 4
5 6 7 8
E . . F
B
3 1

my output:
28
12 BB (4,3) (4,1)
...

4 3
AA AA BB
CC .. BB
CC DD DD
.. .. EE
EE
1 1

4 3
WT WT ST
WR .. ST
WR SR SR
.. .. SK
SK
1 1

EE (3,2) (3,0)
DD (2,1) (3,1)
BB (0,2) (1,2)
AA (0,0) (0,1)
CC (1,0) (0,0)
EE (3,0) (1,1)

2 4
A A B B
C C . .
A
1 2

5
B (0,2) (1,2)
A (0,0) (0,2)
C (1,0) (0,0)
B (1,2) (1,0)
A (0,2) (1,2)

mein output:
now same
solved: endlosschleife, weil target block anomymisierter stein ist -> 
das muss verhindert werden - target darf nicht anonym sein!
target als index 0 eingebracht und reduce anonymisiertt index 0 nicht

is solved by function reduce applied to key of visited map:
needs to anonymize f.e. A and C, D, F, which are equal (2 vertical) not to be differentiated
as different puzzles, to shorten run time (could not be solved in hours before)
(46 seconds with uncompletesolution causing results, which are not shortest)
(4:23 with slow, but correct solution)
(2:10 after eliminating p == parent writing back)
5 4
A B B C
A B B C
D E E F
D G H F
I . . J
B
3 1

testcase 14:
6 6
A B B C C .
. E E D C .
. E . D . F
. . . . F F
I . . H . .
I I I H . .
A
3 5

2
F (2,4) (4,4)
A (0,0) (3,5)

meins noch zu viel: (zuerst muss F ziehen)
A (0,0) (3,3)
F (2,4) (1,4)
A (3,3) (3,5)
fixed: srchTgtWin vor DFS gesetzt, um zu prüfen, ob Target nicht schon ins Ziel kommt

testcase 15:
6 6
D K K L S S
. P T L S S
. P T T R R
. P G G G R
. B Y Y M M
. B Y M M M
A
5 5

mein output: (noch zu viele)
17

BlockArray: array (0,10) [(0,((0,1),[(0,1),(0,2),(1,1),(1,2)])),(1,((0,0),[(0,0),(1,0)])),(2,((0,3),[(0,3),(1,3)])),(3,((2,0),[(2,0),(3,0)])),(4,((2,1),[(2,1)])),(5,((2,2),[(2,2)])),(6,((2,3),[(2,3),(3,3)])),(7,((3,1),[(3,1)])),(8,((3,2),[(3,2)])),(9,((4,0),[(4,0)])),(10,((4,3),[(4,3)]))]
reduced Block List: [[[(0,1),(0,2),(1,1),(1,2)]],[[(2,1)],[(2,2)],[(3,1)],[(3,2)],[(4,0)],[(4,3)]],[[(0,0),(1,0)],[(0,3),(1,3)],[(2,0),(3,0)],[(2,3),(3,3)]]]
-}
{-
[".A..","AB.C","...C"] Target: "C" (0,0) (0,0): '.' Blocks: "ABC"
BlockMap: fromList [('A',((0,0),[(0,1),(1,0)])),('B',((1,1),[(1,1)])),('C',((1,3),[(1,3),(2,3)]))]
BlockArray: array (0,3) [(0,((0,0),[(0,0),(3,4)])),(1,((0,0),[(0,1),(1,0)])),(2,((1,1),[(1,1)])),(3,((1,3),[(1,3),(2,3)]))]
-}
