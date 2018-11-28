{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Data.List (elemIndices, minimum, (\\), notElem, all, sort, repeat, elemIndex, sortBy)
import qualified Data.Set as Set   -- (insert, member, empty)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Foldable
import Control.Arrow
import Data.Bits
import Data.Int
import qualified Data.Array as A
import qualified Data.Maybe as Maybe
import qualified ArrayExtensions as AE

-- 2 coordinate variables y and x are brought to 1 Int8
-- upper 4 bits is y, lower 4 bits is x 
-- limits maximum coordinates to 16x16, while 6x6 would
-- be sufficient based on hackerranks preconditions
type Coord = Int8
type BlkIdx = Int
type Coord' = (Int, Int)
type Bla = Int      -- absolute Shape coordinates in box - "pixel" is a bit
type Blr = Int      -- relative Shape - bit 20-23 y, bit 16-19 x, bit 0-15 Shape
type Block = (Coord, Shape)
type Box = A.Array BlkIdx Bla
type Move = (BlkIdx, (Coord, Coord))

--   64 bits to store maximum 4x4 Block
type Shape = Int16
--instance Num Shape => Num (a -> Shape) where
--      negate      = fmap negate
--      (+)         = liftA2 (+)
--      (*)         = liftA2 (*)
--      fromInteger = pure . fromInteger
--      abs         = fmap abs
--      signum      = fmap signum

type PArray = A.Array Int (Coord', [Coord'])
type VMap = M.Map VMapKey ((Int,PArray), (Int,PArray))
type VMapKey = [Coord']
-- Vector addition.
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
sub (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
getDir (x, y) (x1, y1) = (dir (x - x1),dir (y - y1))
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
xMax = 3  -- maximum size of block
-- convert 2 coordinate variables to one Int8
-- upper half byte y, lower half byte x
coor2Int :: Coord' -> Coord
--coor2Int (y, x) = fromIntegral $ x .|. (rotate y 4)
coor2Int (y, x) = fromIntegral $ y * 16 + x
-- it is equivalent y*16+x, but needs more base knowledge :)
-- coor2Int (y, x) = fromIntegral $ ((0 .|. y) `rotate` 4 ) .|. x

-- convert Int8 to 2 coordinate variables
int2Coor :: Coord -> Coord'
int2Coor int = (fromIntegral int) `divMod` 16

int2y :: Coord -> Int
int2y int = (fromIntegral int) `div` 16
int2x int = (fromIntegral int) `mod` 16

-- encode shapes into numbers
coords2Shape :: (Foldable f) => f Coord' -> Shape
coords2Shape = foldr (\(y,x) -> (`setBit` (xMax*y+x))) 0

-- convert from a list of coordinates to a block
coords2Block :: [Coord'] -> Block
coords2Block xs = (coor2Int topLeft, (coords2Shape_ xs))
    where
    topLeft@(yt,xt) = (minimum . map fst &&& minimum . map snd) xs
    coords2Shape_ = foldr (\(y,x) -> (`setBit` (xMax*(y - yt)+(x - xt)))) 0

--shape2Coord x = foldr (\z y -> if testBit x z then (add topLeft (divMod z xMax) : y) else y) [] [0..(xMax^2-1)]

-- convert from a block to a list of coordinates
block2Coord's :: Block -> [Coord']
block2Coord's (tLInt, shape) = shape2Coords shape
    where
    shape2Coords x = foldr (\z y -> if testBit x z 
                                    then (add topLeft (divMod z xMax) : y)
                                    else y) [] [0..(xMax^2-1)]
    topLeft = int2Coor tLInt

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

-- element 0 of blockarray holds dimensions of puzzle ((0,0), [(0,0),(y,x)])
createBlockArray :: Ord a => [a] -> [[a]] -> PArray --A.Array Int ((Int, Int), [(Int, Int)])
createBlockArray blk pz =   let blM = createBlockMap blk pz
                                getL x = blM M.! x 
                                arrL = zip [0..] (map getL blk)
                            in A.array (0,length blk - 1) arrL

-- Is this a winning position? target has index 0
--isWin :: (Eq a, Num i, A.Ix i) => a -> A.Array i (a, b) -> Bool
isWinT :: Coord' -> (Int, PArray) -> Bool
isWinT goal (blk,pz) = goal == (fst $ pz A.! 0)
isWin :: Coord' -> PArray -> Bool
isWin goal pz = goal == (fst $ pz A.! 0)

-- Try to move the blk in puzzle in direction dir. Return Just the new
-- puzzle, or Nothing if the move isn’t legal.
moveBlk :: Coord' -> PArray -> Int -> [Coord'] -> [PArray]
moveBlk rlc pz blk dirs_ = go dirs_ where
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
-- each blk is moved separatly in DFS
allMovesD :: Foldable t =>
    Int -> b -> Coord' -> t (t1, PArray) -> [(PArray, b)]
allMovesD blk parent rlc pzs = concatMap allMoves1 pzs where -- uniqFstTup $
    allMoves1 (_,pz) = zip (moveBlk rlc pz blk dirs) (repeat parent)
-- not used - just for testing^
-- Given a puzzle, return the list of different
-- puzzles that are reachable from it (father) in exactly one move.
--allMoves1_ :: Coord' -> PArray -> [PArray]
allMoves1_ rlc pz = [moveBlk rlc pz blk dirs |
                blk <- A.indices pz]

{-   breadth first search  (simple - without storing parents)
1. Define a node to begin search; store it to queue
2. Take a node from beginning of queue.
    If found searched element, end and return "found"
    else add all followers of this node to end of queue
3. If queue is empty, all nodes were visited; end search and return "not found"
4. Repeat from step 2.        
-}

-- Find equal blocks (f.e. two steps vertical), sort and add them to list each
-- use this as key for map, reducing to maps with same pattern only
-- do not reduce target! target has index 0
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
        blA = createBlockArray bl pz
        isOneBlk (i, x) = (i, (length $ snd x) == 1)
        goal = (goalL !! 0, goalL !! 1)
    putStr $ findPuzzles (m, n) goal bl blA
--    putStr ""
{-
    putStrLn $ "Block Array: " ++ show blA
    putStrLn $ show p ++ "  Target: " ++ show targS ++ " " ++ show goal ++ "  (0,0): " ++ show ((p !! 0) !! 0)
        ++ "  Blocks: " ++ show bl ++ "  Targetindex: " ++ show targIdx ++ " changed to 0"
    putStrLn $ "reduced Block List: " ++ show (reduce blA)
    putStrLn $ "BlockMap: " ++ show blM
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
