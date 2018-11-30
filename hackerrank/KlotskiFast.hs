{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.List (elemIndices, minimum, (\\), notElem, all, sort, repeat, elemIndex, sortBy)
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
import qualified ArrayExtensions as AE
import Prelude hiding ( minimum, concat, notElem, concatMap, all, mapM_, foldr)
-- 2 coordinate variables y and x are brought to 1 Int8
-- upper 4 bits is y, lower 4 bits is x 
-- limits maximum coordinates to 16x16, while 6x6 would
-- be sufficient based on hackerranks preconditions
type Coord = Int
type BlkIdx = Int
type Coord' = (Int, Int)
-- change from relative to abs shape coordinates by shifting fCoor bits
-- fCoor = y * n + x
type BlockA = Int  -- absolute shape
type BlockR = Int      -- relative Shape - bit 20-23 y, bit 16-19 x, bit 0-15 Shape
type BlockR' = (Coord', Shape)
type Block = (Coord, Shape)
-- 0-3 x blk0, 4-7 y blk0 ... length blk - 1
-- at length blk * 8: last moved block index
-- at (length blk + 1) * 8: target y, target x
type BoxKey = Integer
-- bits 0-7 Blockindex, bits 8-11 to x, bits 12-15 to y,
--                      bits 16-19 from x, bits 20-23 from y
-- not used, because would mean the need of handling whole move history in all variants
type Move = Int32 --(BlkIdx, (Coord, Coord))
--   16 bits to store maximum 4x4 Block
type Shape = Int
-- index 0 target block
-- last index is last moved blockindex; Arrangement bits:
-- 0-7 last block index in BlockArray, 8-11 x block 0, 12-15 y block 0, ...
type Box = Integer  -- just block positions, except index of last moved block
type BSArray = A.Array Int Shape   -- block shape array
type PArray = A.Array Int (Coord', [Coord'])
type BMap = M.Map BoxKey (Box, Box)
type VMap = M.Map VMapKey ((Int,PArray), (Int,PArray))
type VMapKey = [Coord']

-- put block position to box
putBlP :: Box -> Coord' -> BlkIdx -> Box
putBlP box pos idx = box .&. mask .|. (toInteger $ coor2Int pos) `shiftL` offset
    where
    mask = complement $ 0xFF `shiftL` offset -- for erase old
    offset = idx * 8

-- get block position from box
getBlkP :: Box -> Int -> Coord'
getBlkP box idx = int2Coor $ fromInteger $ box `shiftR` offset .&. mask
    where
    mask = 0xFF
    offset = idx * 8

-- put block position to box
putBlIP :: Box -> Coord -> BlkIdx -> Box
putBlIP box pos idx = box .&. mask .|. (toInteger pos) `shiftL` offset
    where
    mask = complement $ 0xFF `shiftL` offset -- for erase old
    offset = idx * 8

-- get block position from box
getBlkIP :: Box -> Int -> Coord
getBlkIP box idx = fromInteger $ box `shiftR` offset .&. mask
    where
    mask = 0xFF
    offset = idx * 8

-- get coordinates of BlockR
getBlkCo :: BlockR -> Coord'
getBlkCo block = int2Coor $ block `shiftR` offset .&. mask
    where
    mask = 0xFF
    offset = 16

-- get shape of BlockR
getBlkS :: BlockR -> Shape
getBlkS block = block .&. mask
    where
    mask = 0xFFFF

-- Vector addition.
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
sub (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
-- getDir from to
getDir (x, y) (x1, y1) = (dir (x1 - x),dir (y1 - y))
    where
    dir x_
        | x_ < 0 = (-1)
        | x_ > 0 = 1
        | x_ == 0 = 0

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

-- get x lines
getLn :: Int -> IO [String]
getLn 0 = return []
getLn x = do
    cu <- getLine
    cs <- getLn (x - 1)
    return ([cu] ++ cs)

nMax = 6  -- maximum size of puzzle
xMax = 3  -- maximum size of block - must be <= 4 !!
-- convert 2 coordinate variables to one Int
-- upper half byte y, lower half byte x
coor2Int :: Coord' -> Coord
--coor2Int (y, x) = fromIntegral $ x .|. (shift y 4)
coor2Int (y, x) = y * 16 + x

-- convert Int to 2 coordinate variables
int2Coor :: Coord -> Coord'
int2Coor int = int `divMod` 16

int2y :: Coord -> Int
int2y int = int `div` 16
int2x int = int `mod` 16

-- encode coordinates to shape
coords2Shape :: (Foldable f) => Int -> f Coord' -> Shape
coords2Shape n = foldr (\(y,x) -> (`setBit` (n*y+x))) 0
-- shape to coordinates (absolute) - to convert topLeft (1 set bit)
shape2Coord :: Int -> Shape -> Coord'
shape2Coord n shape = shape `divMod` n
-- convert from a list of coordinates to a block
-- BlockR: relative Shape - bit 20-23 y, bit 16-19 x, bit 0-15 Shape
coords2Block_ :: Int -> Int -> [Coord'] -> BlockR
coords2Block_ m n xs = inty .|. intx .|. (shiftR shape topLeftS)
    where
    topLeftS = go shape 0
    inty = shiftL (topLeftS `div` n) 20
    intx = shiftL (topLeftS `mod` n) 16
    go :: Int -> Int -> Int
    go shap x
        | isNotAtTop = go (shiftR shap n) (x + n)
        | isInTopEdge = x
        | otherwise = go (shiftR shap 1) (x + 1)
        where
        isNotAtTop = shap .&. tREP == 0
        isInTopEdge = shap .&. lREP /= 0
    --topRowEdgePattern
    tREP = tREP_ n
    --leftRowEdgePattern
    lREP = lREP_ m n
    shape = coords2Shape_ xs
    coords2Shape_ = foldr (\(y,x) -> (`setBit` (n*y+x))) 0

lREP_ m n = go 0 m where 
    go mkl 0 = mkl
    go mkl m_ = go (mkl `setBit` ((m_ - 1) * n)) (m_ - 1)

rREP_ m n = go 0 m where
    go mkr 0 = mkr
    go mkr m_ = go (mkr `setBit` ((m_ - 1) * n + n - 1)) (m_ - 1)

tREP_ n = go 0 n where
    go mkt 0 = mkt
    go mkt n_ = go (mkt `setBit` (n_ - 1)) (n_ - 1)

bREP_ m n = go 0 n where
    go mkb 0 = mkb
    go mkb n_ = go (mkb `setBit` ((n_ - 1) + n * (m - 1))) (n_ - 1)

-- not used
edgeFrame_ :: Int -> Int -> Int
edgeFrame_ m n = lREP_ m n .|. rREP_ m n .|. tREP_ n .|. bREP_ m n

coords2Block' :: Int -> Int -> [Coord'] -> BlockR
coords2Block' m n xs = inty .|. intx .|. (shiftR shape topLeftS)
    where
    topLeftS = tLS shape
    inty = shiftL (topLeftS `div` n) 20
    intx = shiftL (topLeftS `mod` n) 16
    tLS shap = vertical + horizontal
        where
        vertical = head $ filter isAtTop [0,n..]
        horizontal = head $ filter isInTopEd [0..]
        isAtTop s = (shap `shiftR` s) .&. tREP /= 0
        isInTopEd s = (shap `shiftR` (s + vertical)) .&. lREP /= 0
    --topRowEdgePattern
    tREP = tREP_ n
    --leftRowEdgePattern
    lREP = lREP_ m n
    shape = coords2Shape_ xs
    coords2Shape_ = foldr (\(y,x) -> (`setBit` (n*y+x))) 0

coords2Block :: Int -> [Coord'] -> BlockR
coords2Block n xs = (shift (coor2Int topLeft) 16) .|. (coords2Shape_ xs)
    where
    topLeft@(yt,xt) = (minimum . map fst &&& minimum . map snd) xs
    coords2Shape_ = foldr (\(y,x) -> (`setBit` (n*(y - yt)+(x - xt)))) 0

go x = foldr (\x -> (+ coords2Block 4 [(3,1),(2,2),(2,3),(3,2),(3,3),(4,1),(4,2),(4,3)])) 0 [1..x]
go_ x = foldr (\x -> (+ coords2Block_ 5 4 [(3,1),(2,2),(2,3),(3,2),(3,3),(4,1),(4,2),(4,3)])) 0 [1..x]
go' x = foldr (\x -> (+ coords2Block' 5 4 [(3,1),(2,2),(2,3),(3,2),(3,3),(4,1),(4,2),(4,3)])) 0 [1..x]
-- convert from a block to a list of coordinates
-- n is horizontal box dimension (from m x n)
block2Coord's :: Int -> BlockR -> [Coord']
block2Coord's n blkR = shape2Coords
    where
    shape = 0xFFFF .&. blkR
    topLeft = int2Coor $ (shift blkR (-16))
    shape2Coords = foldr (\z y -> if testBit shape z 
                                    then (add topLeft (divMod z n) : y)
                                    else y) [] [0..(n^2-1)]

-- absolute shape
shape2Coords' :: Int -> BlockA -> [Coord']
shape2Coords' n shape = foldr (\z y -> if testBit shape z 
                                then (divMod z n) : y
                                else y) [] [0..(n^2-1)]

-- change from relative Box, blSAr to absolute block (shape)
--toAbsBA :: Int -> BlockR -> BlockA
--toAbsBA n blkR = shape `shiftL` (y * n + x)
--    where
--    (y,x) = getBlkCo blkR
--    shape = getBlkS blkR

-- change from relative (Coor,Shape) to absolute block (shape)
toAbsB :: Int -> BlockR -> BlockA
toAbsB n blkR = shape `shiftL` (y * n + x)
    where
    (y,x) = getBlkCo blkR
    shape = getBlkS blkR

ofB'2BlA :: Int -> BlockR' -> BlockA
ofB'2BlA n ((y,x),shape) = shape `shiftL` (y * n + x)

dec2bin x = concatMap show $ reverse $ decToBin' x
  where
    decToBin' 0 = [0]
    decToBin' y = let (a,b) = quotRem y 2 
                    in [b] ++ decToBin' a

createBlockMapF :: Ord a => Int -> [a] -> [[a]] -> M.Map a BlockR
createBlockMapF n blk pz = go blk 0 0 M.empty where
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
            | M.member b blMap = block2Coord's n $ blMap M.! b
            | otherwise = []
        isB = b == pz !! pl !! pc
        el = l ++ [(pl, pc)]
        bleMap = M.insert b (coords2Block n el) blMap
        blPMap = M.insert b (coords2Block n l) blMap

createBlockMap :: Ord a => [a] -> [[a]] -> M.Map a ((Int, Int), [(Int, Int)])
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
createBlockArrays :: Ord a => Int -> [a] -> [[a]] -> (Box, BSArray)
createBlockArrays n blk pz =    
    let blM = createBlockMapF n blk pz
        getBlR x = blM M.! x 
        arrL = zip [0..] (map (getBlkS . getBlR) blk)
        blockShapeArr = A.array (0, len - 1) arrL
        blockPosArr = box_ 0 0 where
            box_ bx nr
                | nr == len = putBlIP bx 0 len
                | otherwise = box_ (putBlP bx pos nr) (nr+1)
                where
                pos = getBlkCo $ getBlR (blk !! nr)
        len = length blk
--        blockCoordsL = map (coor2Int $ getBlkCo) blk
    in (blockPosArr, blockShapeArr)

-- element 0 of blockarray holds target block
createBlockArray :: Ord a => [a] -> [[a]] -> PArray
createBlockArray blk pz =
    let blM = createBlockMap blk pz
        getL x = blM M.! x 
        arrL = zip [0..] (map getL blk)
    in A.array (0,length blk - 1) arrL

-- Is this a winning position? target has index 0
--isWin :: (Eq a, Num i, A.Ix i) => a -> A.Array i (a, b) -> Bool
isWinF :: Coord' -> Box -> Bool
isWinF goal box = goal == getBlkP box 0
isWinT :: Coord' -> (Int, PArray) -> Bool
isWinT goal (blk,pz) = goal == (fst $ pz A.! 0)
isWin :: Coord' -> PArray -> Bool
isWin goal pz = goal == (fst $ pz A.! 0)

-- Try to move the blk in puzzle in direction dir.
-- Return puzzles with possible moved block - can be empty list
moveBlk :: Coord' -> PArray -> Int -> [PArray]
moveBlk rlc pz blk = go dirs where
    go [] = []
    go dirs''
        | check = (pz A.// [(blk, (blockPos, poses2))]) : go (tail dirs'')
        | otherwise = go (tail dirs'') where
        dir = head dirs''
        check = newNotInOtherBlks && newNotOut
        poses = snd $ pz A.! blk;  poses2 = map (add dir) poses
        new = poses2 \\ poses
        blockPos = (\(y, x) -> (minimum y, minimum x)) $ unzip poses2
        xMax = snd rlc - 1;  yMax = fst rlc - 1  -- right lower corner
        blks = A.indices pz
        otherBlkPoses = concatMap getPoses otherBlks;  otherBlks = blks \\ [blk]
        getPoses x = snd $ pz A.! x
        newNotInOtherBlks = all (==True) (map checkIfNotIn new)
        checkIfNotIn x = notElem x otherBlkPoses
        newNotOut = case dir of
            (0, -1) -> all (\(y,x) -> x >= 0) new
            (0, 1) -> all (\(y,x) -> x <= xMax) new
            (-1, 0) -> all (\(y,x) -> y >= 0) new
            (1, 0) -> all (\(y,x) -> y <= yMax) new

-- Given a list of puzzles, return the list of different
-- puzzles (with each parent in a tuple) that are reachable from them in exactly one move.
--allMoves :: Coord' -> [PArray] -> [(PArray, PArray)]
dirs = [(0, -1), (0, 1), (-1, 0), (1, 0)]
dirs_ n = [((0, -1),-1), ((0, 1),1), ((-1, 0),-1 * n), ((1, 0),n)]
-- each blk is moved separatly in DFS
allMovesD :: Foldable t =>
    Int -> b -> Coord' -> t (t1, PArray) -> [(PArray, b)]
allMovesD blk parent rlc pzs = concatMap allMoves1 pzs where
    allMoves1 (_,pz) = zip (moveBlk rlc pz blk) (repeat parent)
-- not used - just for testing^
-- Given a puzzle, return the list of different
-- puzzles that are reachable from it (father) in exactly one move.
--allMoves1_ :: Coord' -> PArray -> [PArray]
allMoves1_ rlc pz = [moveBlk rlc pz blk |
                blk <- A.indices pz]

-- 4-7 target y, 0-3 target x   -- leave it in box
-- 12-15 y blk1, 8-11 x blk1,... 
-- at maxBd * 8: last moved block index -- leave it in box
-- sorted by block shapes and then their positions, only positions used
reduceF :: BSArray -> Box -> BoxKey
reduceF blSAr box = posL2box sortedPossL
    where
    blSAL = A.elems blSAr  -- shape element list
    blSL = tail blSAL -- process without target
    maxBd = snd (A.bounds blSAr)

    sortedPossL = map snd $ sort $ zip blSL posL0box
    posL2box posL = foldr (\(pos,idx) box_ -> putBlIP box_ pos idx) box (zip posL [1..])
    posL0box = map (getBlkIP box) [1 .. maxBd - 1]  -- posL0box without target
    --map (getBlkP 20302216961863071546456865382688) [0..13]

-- test running funtion x times to see runtime
goL f x = go [] x where
    go y 0 = y
    go y x = go (f x) (x - 1)
goN f x = go 0 x where
    go y 0 = y
    go y x = go (f x) (x - 1)

quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

mergesort'merge :: (Ord a) => [a] -> [a] -> [a]
mergesort'merge [] xs = xs
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)
    | (x < y) = x:mergesort'merge xs (y:ys)
    | otherwise = y:mergesort'merge (x:xs) ys
 
mergesort'splitinhalf :: [a] -> ([a], [a])
mergesort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort'splitinhalf xs

lrot (x:xs) = xs ++ [x]
repRot l = l: (repRot (lrot l))
-- goL (\x -> map quicksort $ take x $ repRot [4,4,6,9,6,3,8,3,7,2,1,7]) 1000000

reduce :: BlkIdx -> PArray -> VMapKey
reduce blk pz = reducedL
    where
    blsAL = A.elems pz  -- (pos, blockList) element lists
    blsL = tail blsAL -- process without target
    (targetPos, targetBlL) = (pz A.! 0)
    -- sorted by block shapes and then their positions, only positions used
    blMTo0 blL = map snd $ sort $ map (\(p,l) -> (map (sub p) l, p)) blL
    reducedL = (blk,0) : targetPos : (blMTo0 blsL)
    

-- Add puzzle p to the visited map with its parent.
-- Return the updated map and Just p if p wasn’t previously visited,
-- Nothing otherwise. Use addPositionM for multiple step moves mode instead of steps
--addPosition :: VMap -> (PArray, PArray) -> (VMap, Maybe PArray)
addPosition_ blkpa blk visited (p, parent) = (visited', q)
    where 
    old_p_parent = M.lookup (reduce blk p) visited
    (visited', q) = case old_p_parent of
        Nothing -> (M.insert (reduce blk p) ((blk, p), (blkpa,parent)) visited, Just p)
        Just _ -> (visited, Nothing)
        where
        isEqual = p == parent
addPosition blkpa blk visited (p, parent) = (visited', q)
    where 
    old_p_parent = M.lookup (reduce blk p) visited
    (visited', q) = case old_p_parent of
        Nothing -> (M.insert (reduce blk p) ((blk, p), (blkpa,parent)) visited, Just p)
        Just _ -> (visited, Nothing)
        where
        isEqual = p == parent

-- take visited map and list of puzzles with each parent to store to visited map
-- return updated map and new puzzles (not previously already in visited map)
-- for processing next level
--addPositions :: VMap -> [(PArray, PArray)] -> (VMap, [PArray])
addPositions blkpa blk visited [] = (visited, [])
addPositions blkpa blk visited ((p, parent):ps) = (visited'', qs)
    -- do not process again, if p == parent, but store as visited
    where qs = case q of Just p' -> case () of
                            _ | p' == parent -> ps'
                            _ | otherwise -> (blk, p'):ps'
                         Nothing -> ps'
          (visited', ps') = addPositions blkpa blk visited ps
          (visited'', q) = addPosition blkpa blk visited' (p, parent)

-- Given the map of visited puzzles and the list
-- of current puzzles, return an updated map with all next moves
--newPositions :: Coord' -> VMap -> [PArray] -> (VMap,[PArray])
newPositionsD blkpa parent blk rlc visited curr_pzs = 
    addPositions blkpa blk visited (allMovesD blk parent rlc curr_pzs)

-- Go level by level (level = all puzzles reachable with 1 step) 
-- through all reachable puzzles from starting puzzle
--findPuzzles :: Coord' -> Coord' -> [[Char]] -> PArray -> [Char]
findPuzzlesF rlc goal blkL (start, blSAr) = 
        go (M.singleton (reduceF blSAr start) (start, root)) [start] 0 where
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
        goM visitedM [] allPz1MA c
            -- for testing each level and show map (current, parent) or just next puzzles
            | c < 579999 = go visitedM allPz1MA c
--            -- show puzzles of allPz1MA with their parents
            | otherwise = "\n" ++ show (length pz_parentL) ++ "\n" ++ 
--                    concatMap (\(p,pa) -> showPz p ++
--                    " block, parent:\n" ++ showPz pa) pz_parentL
                    concatMap (\(p,pa) -> showPz p ++ show b ++
                    " block, parent block: " ++ show blkpa ++ "\n" ++ showPz pa) pz_parentL
            where
            blkpa = getBlkP (maxBd + 1); b = getBlkP (maxBd + 1)
            pz_parentL = map createPPar allPz1MA
--            createPPar (blk, pz) = (pz, parent blk pz) where
            createPPar pz = (pz, parent) where
                parent = visitedM M.! (reduce blSAr pz)
        goM visitedM (pz : pzsMR) allPz1MA c 
                = goD visitedM geoBlkIL [pz] allPz1MA c
        -- Depth first search with each block, target last
            where
            goD vstd [] _ allPz1M c -- = goM vstd pzsMR allPz1M c
                | any (isWinF goal) allPz1M = out vstd allPz1M
--                | any (isWinF goal) allPz1M = outAll vstd allPz1M
                | otherwise = goM vstd pzsMR allPz1M (c + 1)
            goD vstd blkL [] allPz1M c = goD vstd (tail blkL) [pz] allPz1M c
            goD vstd blkL pzsD allPz1M c  = goD visitedD blkL pzs'' (pzs'' ++ allPz1M) c
                where
                blk = head blkL
                isTarget = blk == 0
                (visitedD, pzs'') = newPositionsD pz vstd pzsD
                currBlkRShape = blSAr A.! blk

                -- Try to move the blk in puzzle in direction dir.
                -- Return puzzles with possible moved block - can be empty list
                moveBlkF :: BSArray -> Box -> [Box]
                moveBlkF box = go (dirs_ n) where
                    othersBox = foldr fldF 0 othPossShps
                    fldF ((y,x), shape) box_ = box_ .|. (shape `shiftL` (y * n + x))
                    -- build abs shape of all blocks except blk
                    othPossShps = zip otherPoses otherShapes
                    otherShapes = blSL \\ [currBlkRShape]
                    otherPoses = posL0box \\ [currBlkPos]
                    posL0box = map (getBlkP box) [0 .. maxBd - 1]
                    currBlkPos@(y,x) = getBlkP box blk
                    currBlkAShape = currBlkRShape `shiftL` (y * n + x)
                    go [] = []
                    go dirs''
                        | check = (putBlP box (add currBlkPos dir') blk) : go (tail dirs'')
                        | otherwise = go (tail dirs'') where
                        (dir', dir) = head dirs''
                        check = newNotInOtherBlks && newNotOut
                        new = currBlkAShape `shift` dir
                        newNotInOtherBlks = new .&. othersBox == 0
                        chk pat = currBlkAShape .&. (pat m n) == 0
                        newNotOut = case dir' of
                            (0, -1) -> chk lREP_
                            (0, 1) -> chk rREP_
                            (-1, 0) -> currBlkAShape .&. (tREP_ n) == 0
                            (1, 0) -> chk bREP_
                allMovesF parent pzs = concatMap allMoves1 pzs where
                    allMoves1 pz = zip (moveBlkF pz) (repeat parent)
                addPositionF blk visited (p, parent) = (visited', q)
                    where 
                    old_p_parent = M.lookup (reduce blSAr p) visited
                    (visited', q) = case old_p_parent of
                        Nothing -> (M.insert (reduce blSAr p) (p, parent) visited, Just p)
                        Just _ -> (visited, Nothing)
                        where
                        isEqual = p == parent
                addPositionsF visited [] = (visited, [])
                addPositionsF visited ((p, parent) : ps) = (visited'', qs)
                    -- do not process again, if p == parent, but store as visited
                    where
                    qs = case q of
                        Just p' -> case () of
                            _ | p' == parent -> ps'
                            _ | otherwise -> (addLastBlock p'):ps'
                        Nothing -> ps'
                    (visited', ps') = addPositionsF visited ps
                    (visited'', q) = addPositionF visited' (p, parent)
                    -- last block in box p' packen
                    addLastBlock x = putBlIP x blk (maxBd + 1)

                -- Given the map of visited puzzles and the list
                -- of current puzzles, return an updated map with all next moves
                newPositionsD parent visited curr_pzs = 
                    addPositionsF visited (allMovesF parent curr_pzs)
        
    maxBd = snd (A.bounds blSAr)
    blSL = A.elems blSAr
    (m, n) = rlc  -- box dimensions

    v = m - 1; h = n - 1  --vMax, hMax
    geoBlkIL = geoSorIdxL start
    --assumption: best to sort by size of block ascending and then 
    --sort them in shortest geometrical distance (sortBy position)
    geoSrt p = sortBy (\(_,(y1,x1)) (_,(y2,x2)) 
        -> compare ((v-y1)^2+(h-x1)^2) ((v-y2)^2+(h-x2)^2)) p
    geoSorIdxL p = map (\(idx,(pos,posL)) -> idx) $ geoSrt (assocs_ p)
    assocs_ x = zip [0..maxBd] (map (getBlkP x) [0..])
--    blkIL = A.indices start
    winPz pzss = filter (isWinF goal) pzss
    -- map list of puzzles from start to winning puzzle
    getPathL vstd pzss = go [head pzss] where
        go pathL
            | parent == root = pathL 
            | otherwise = go (parent : pathL) where
            (_, parent) = vstd M.! (reduce blSAr (head pathL))
    -- create list of blocks and their single step moves by finding 
    -- the different blocks of neighboring puzzles of solution path
    blkStepsL vstd pzss = map blkMove blkLNeighborsL where
        posAL = map assocs_ (getPathL vstd pzss)
        blkLNeighborsL = zip posAL (tail posAL)
        blkMove (l1, l2) = head $ map cvtToBlkMvs $ filter diffItems $ zip l1 l2 where
            cvtToBlkMvs ((i, blkP), (i1, blkP1)) = ((blkL !! i), blkP, blkP1)
            diffItems (x, y) = x /= y
    cvtToOut blkmvL = show len ++ "\n" ++ (unlines $ map str blkmvL)
        where
        len = length blkmvL
        str (x,y,z) = x ++ " " ++ show y ++ " " ++ show z
--    root = A.array (A.bounds start) [(blki,((-1::Int,0::Int),[(0::Int,0::Int)])) | blki <- A.indices start]
    root = 0
    showPz pz = (unlines $ map unwords $ map (map getBlkDgt) allPosL) ++ "\n"
        where
        allPosL = [[(y,x) | x <- [0..h]] |  y <- [0..v] ]
        blkIL = A.indices blSAr
        posL blkidx = shape2Coords' n blkAShape where
            blkAShape = blkRShape `shiftL` (y * n + x)
            (y,x) = getBlkP pz blkidx
            blkRShape = blSAr A.! blkidx
        getBlkDgt pos
            | idxL == [] = map (const '.') (head blkL)
            | otherwise = blkL !! (snd $ head idxL)
            where
            idxL = filter isPosInA $ zip (map posInA blkIL) [0..]
            isPosInA (x, _) = x == True
            posInA x = elem pos (posL x)

findPuzzles rlc goal blkL start = 
        go (M.singleton (reduce blkk start) ((blkk,start), (blkk,root))) [(blkk, start)] 0 where
    blkkIL = A.indices start
    blkk = head blkkIL
    go _ [] c = "sorry, no solution found after cycles: " ++ show c
    go visited pzs c -- = goM visited pzs [] (c + 1) -- = goST visited pzs []
        | any (isWinT goal) pzs = out visited pzs
--        | any (isWinT goal) ps = outAll visited pzs
        | otherwise = goM visited pzs [] 0 -- goST visited pzs []
        where
        cvt_bML vmap x = cvtToOut (blkStepsL vmap x)
        outAll vmap pzL = concatMap (cvt_bML vmap) [winPz pzL]
        out vmap pzL = cvtToOut (blkStepsL vmap (minimum $ [winPz pzL]))

        -- getting next level moves
        goM visitedM [] allPz1MA c
            -- for testing each level and show map (current, parent) or just next puzzles
            | c < 579999 = go visitedM allPz1MA c
--            -- show puzzles of allPz1MA with their parents
            | otherwise = "\n" ++ show (length pz_parentL) ++ "\n" ++ 
--                    concatMap (\(p,pa) -> showPz p ++
--                    " block, parent:\n" ++ showPz pa) pz_parentL
                    concatMap (\((b,p),(blkpa, pa)) -> showPz p ++ show b ++
                    " block, parent block: " ++ show blkpa ++ "\n" ++ showPz pa) pz_parentL
            where
            pz_parentL = map createPPar allPz1MA
--            createPPar (blk, pz) = (pz, parent blk pz) where
            createPPar (blk, pz) = ((blk,pz), (blkpa, parent)) where
                (blkpa, parent) = snd $ visitedM M.! (reduce blk pz)
        goM visitedM (pa@(blkpa,pz) : pzsMR) allPz1MA c 
                = goD visitedM geoBlkIL [pa] allPz1MA c
        -- Depth first search with each block, target last
            where
            goD vstd [] _ allPz1M c -- = goM vstd pzsMR allPz1M c
                | any (isWinT goal) allPz1M = out vstd allPz1M
--                | any (isWinT goal) allPz1M = outAll vstd allPz1M
                | otherwise = goM vstd pzsMR allPz1M (c + 1)
            goD vstd blkL [] allPz1M c = goD vstd (tail blkL) [pa] allPz1M c
            goD vstd blkL pzsD allPz1M c  = goD visitedD blkL pzs'' (pzs'' ++ allPz1M) c
                where
                blk = head blkL
                isTarget = blk == 0
                (visitedD, pzs'') = newPositionsD blkpa pz blk rlc vstd pzsD

        v = fst rlc - 1
        h = snd rlc - 1
        geoBlkIL = geoSorIdxL start
        --assumption: best to sort by size of block ascending and then 
        --sort them in shortest geometrical distance (sortBy position)
        geoSrt p = sortBy (\(_,((y1,x1),_)) (_,((y2,x2),_)) 
            -> compare ((v-y1)^2+(h-x1)^2) ((v-y2)^2+(h-x2)^2)) p
        lenSort x = sortBy (\(_,(_,list1)) (_,(_,list2)) 
            -> compare (length list1) (length list2)) x
        geoSorIdxL p = map (\(idx,(pos,posL)) -> idx) $ geoSrt (A.assocs p)
        blkIL = A.indices start
        winPz pzss = filter (isWinT goal) pzss
        -- map list of puzzles from start to winning puzzle
        getPathL vstd pzss = go [snd $ head pzss] (fst $ head pzss) where
            go pathL lblkpa
                | parent == root = pathL 
                | otherwise = go (parent : pathL) blkpa where
                (_, (blkpa,parent)) = vstd M.! (reduce lblkpa (head pathL))
        -- create list of blocks and their single step moves by finding 
        -- the different blocks of neighboring puzzles of solution path
        blkStepsL vstd pzss = map blkMove blkLNeighborsL where
            pzL = map A.assocs (getPathL vstd pzss)
            blkLNeighborsL = zip pzL (tail pzL)
            blkMove (l1, l2) = head $ map cvtToBlkMvs $ filter diffItems $ zip l1 l2 where
                cvtToBlkMvs ((i, blk), (i1, blk1)) = ((blkL !! i), (fst blk), fst blk1)
                diffItems (x, y) = x /= y
        cvtToOut blkmvL = show len ++ "\n" ++ (unlines $ map str blkmvL)
            where
            len = length blkmvL
            str (x,y,z) = x ++ " " ++ show y ++ " " ++ show z
    root = A.array (A.bounds start) [(blki,((-1::Int,0::Int),[(0::Int,0::Int)])) | blki <- A.indices start]

    showPz pz = (unlines $ map unwords $ map (map getBlkDgt) allPosL) ++ "\n"
        where
        allPosL = [[(y,x) | x <- [0..(snd rlc - 1)]] |  y <- [0..(fst rlc - 1)] ]
        blkIL = A.indices pz
        getBlkDgt pos
            | idxL == [] = map (const '.') (head blkL)
            | otherwise = blkL !! (snd $ head idxL)
            where
            idxL = filter isPosInA $ zip (map posInA blkIL) [0..]
            isPosInA (x, _) = x == True
            posInA x = elem pos (snd (pz A.! x))

rlc = (5,4)
blkL = ["BS","RW","RS","TW","TS","LW","LS","BW","DW","DS","KW","KS","BB"]
showP :: PArray -> [Char]
showP pz = (unlines $ map unwords $ map (map getBlkDgt) allPosL) ++ "\n"
    where
    allPosL = [[(y,x) | x <- [0..(snd rlc - 1)]] |  y <- [0..(fst rlc - 1)] ]
    blkIL = A.indices pz
    getBlkDgt pos
        | idxL == [] = map (const '.') (head blkL)
        | otherwise = blkL !! (snd $ head idxL)
        where
        idxL = filter isPosInA $ zip (map posInA blkIL) [0..]
        isPosInA (x, _) = x == True
        posInA x = elem pos (snd (pz A.! x))

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
        blMF = createBlockMapF n bl pz
        blMFlesbar = map (\(x,y) -> (x,(read (dec2bin y) :: Integer))) (M.toList $ createBlockMapF n bl pz)
        blA = createBlockArray bl pz
        blAF@(blA_, blSA) = createBlockArrays n bl pz
        blA_lesbar = zip [0..length bl] (map (getBlkP blA_) [0..])
        isOneBlk (i, x) = (i, (length $ snd x) == 1)
        goal = (goalL !! 0, goalL !! 1)
    putStrLn $ show pz ++ "  Target: " ++ show targS ++ " " ++ show goal ++ "  (0,0): " ++ show ((p !! 0) !! 0)
        ++ "  Blocks: " ++ show bl ++ "  Targetindex: " ++ show targIdx ++ " changed to 0"
    putStrLn $ "BlockMap: " ++ show blM
    putStrLn $ "BlockMap neu: " ++ show blMF
    putStrLn $ "BlockMap neu (lesbar): " ++ show blMFlesbar
    putStrLn $ "Block Array: " ++ show blA
    putStrLn $ "Block Array neu: " ++ show blA_
    putStrLn $ "Block Array neu (lesbar): " ++ show blA_lesbar
    putStrLn $ "Block Shape Array: " ++ show blSA
    putStr $ findPuzzlesF (m, n) goal bl blAF
    putStr $ findPuzzles (m, n) goal bl blA
--    putStr ""
{-
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
1:52 after optimizing reduce to just [Coord']

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
