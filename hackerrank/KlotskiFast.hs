module Main where
import Data.List (elemIndices, minimum, (\\), notElem, all, sort, repeat, elemIndex, sortBy)
import qualified Data.Set as Set   -- (insert, member, empty)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Maybe as Maybe
import qualified ArrayExtensions as AE

type PArray = A.Array Int ((Int, Int), [(Int, Int)])
type Coord = (Int, Int)
type VMap = M.Map VMapKey (PArray, PArray)
type VMapKey = [[[Coord]]]

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

createBlockMap :: Ord a => [[a]] -> [[[a]]] -> M.Map [a] ((Int, Int), [(Int, Int)])
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
createBlockArray :: Ord a => [[a]] -> [[[a]]] -> PArray --A.Array Int ((Int, Int), [(Int, Int)])
createBlockArray blk pz =   let blM = createBlockMap blk pz
                                getL x = blM M.! x 
                                arrL = zip [0..] (map getL blk)
                            in A.array (0,length blk - 1) arrL

-- Is this a winning position? target has index 0
--isWin :: (Eq a, Num i, A.Ix i) => a -> A.Array i (a, b) -> Bool
isWin :: Coord -> PArray -> Bool
isWin goal pz = goal == (fst $ pz A.! 0)

-- Try to move the blk in in puzzle in direction dir. Return Just the new
-- puzzle, or Nothing if the move isn’t legal.
moveBlk :: Coord -> PArray -> Int -> Coord -> Maybe PArray
moveBlk rlc pz blk dir
    | check = Just $ pz A.// [(blk, (blockPos, poses2))]
    | otherwise = Nothing where
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
allMoves :: Coord -> [PArray] -> [(PArray, PArray)]
allMoves rlc pzs = uniqFstTup $ concatMap allMoves1 pzs where
    allMoves1 pz = zip (Maybe.catMaybes [moveBlk rlc pz blk dir |
                blk <- A.indices pz,
                dir <- dirs]) (repeat pz)
dirs = [(0, -1), (0, 1), (-1, 0), (1, 0)]
-- each blk is moved separatly in DFS
allMovesD parent blk rlc pzs = concatMap allMoves1 pzs where -- uniqFstTup $
    allMoves1 pz = zip (Maybe.catMaybes [moveBlk rlc pz blk dir |
--                blk <- rotate (A.indices pz),
                dir <- dirs]) (repeat parent)
-- not used - just for testing^
-- Given a puzzle, return the list of different
-- puzzles that are reachable from it (father) in exactly one move.
allMoves1_ :: Coord -> PArray -> [PArray]
allMoves1_ rlc pz = Maybe.catMaybes [moveBlk rlc pz blk dir |
                blk <- A.indices pz,
                dir <- dirs]

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
reduce :: PArray -> VMapKey
reduce pz = go blsL M.empty where
    go [] blM            = [targetBlL] : M.elems blM
    go (x@(pos, eL):xs) blM = go xs blSM
        where
        i = redBlM M.! blM1To0
        blM1To0 = map (sub pos) eL
        mby = M.lookup i blM
        blSM = case mby of
            Just old -> M.insert i (sort (old ++ [eL])) blM
            Nothing -> M.insert i ([eL]) blM
    blsAL = A.elems pz
    blsL = tail blsAL -- process without target
    (_, targetBlL) = (blsAL !! 0)
    blMTo0 blL = (\(p,l) -> map (sub p) l) blL
    redBlSet = Set.fromList $ map blMTo0 blsL
    redBlL = Set.toList redBlSet
    redBlM = M.fromList $ zip redBlL [0..]

-- Add puzzle p to the visited map with its parent.
-- Return the updated map and Just p if p wasn’t previously visited,
-- Nothing otherwise. Use addPositionM for multiple step moves mode instead of steps
--addPosition :: VMap -> (PArray, PArray) -> (VMap, Maybe PArray)
addPosition visited (p, parent) = (visited', q)
    where 
    old_p_parent = M.lookup (reduce p) visited
    (visited', q) = case old_p_parent of
        Nothing -> (M.insert (reduce p) (p, parent) visited, Just p)
        Just _ -> (visited, Nothing)

-- take visited map and list of puzzles with each parent to store to visited map
-- return updated map and new puzzles (not previously already in visited map)
-- for processing next level
--addPositions :: VMap -> [(PArray, PArray)] -> (VMap, [PArray])
addPositions visited [] = (visited, [])
addPositions visited ((p, parent):ps) = (visited'', qs)
    where qs = case q of Just p' -> p':ps'
                         Nothing -> ps'
          (visited', ps') = addPositions visited ps
          (visited'', q) = addPosition visited' (p, parent)

-- Given the map of visited puzzles and the list
-- of current puzzles, return an updated map with all next moves
--newPositions :: Coord -> VMap -> [PArray] -> (VMap,[PArray])
newPositionsD parent blk rlc visited curr_pzs = 
    addPositions visited (allMovesD parent blk rlc curr_pzs)

-- Go level by level (level = all puzzles reachable with 1 step) 
-- through all reachable puzzles from starting puzzle
--findPuzzles :: Coord -> Coord -> [[Char]] -> PArray -> [Char]
findPuzzles rlc goal blkL start = 
        go (M.singleton (reduce start) (start, root)) [start] 0 where
    go visited pzs c -- = goM visited pzs [] (c + 1) -- = goST visited pzs []
        | any (isWin goal) pzs = out visited pzs
--        | any (isWin goal) pzs = outAll visited pzs
        | otherwise = goM visited pzs [] 0 -- goST visited pzs []
        where
        cvt_bML vmap x = cvtToOut (blkStepsL vmap x)
        outAll vmap pzL = concatMap (cvt_bML vmap) (winPz pzL)
        out vmap pzL = cvtToOut (blkStepsL vmap (minimum $ winPz pzL))

        -- getting next level moves
        goM visitedM [] allPz1MA c
            -- for testing each level and show map (current, parent) or just next puzzles
            | c < 999999 = go visitedM allPz1MA c
--            -- show puzzles of allPz1MA with their parents
            | otherwise = "\n" ++ show (length pz_parentL) ++ "\n" ++ 
                    concatMap (\x -> (showPz (fst x)) ++ 
                    "parent:\n" ++ showPz (snd x)) pz_parentL
            where
            pz_parentL = map createPPar allPz1MA
            createPPar x = (x, parent x)
            parent x = snd $ visitedM M.! (reduce x)
        goM visitedM (pz : pzsMR) allPz1MA c
            | start == testcase12 = goD visitedM [9,8,5,2,0,1,3,4,7,6] [pz] allPz1MA c
            | otherwise = goD visitedM geoBlkIL [pz] allPz1MA c
        -- Depth first search with each block, target last
            where
            goD vstd [] _ allPz1M c -- = goM vstd pzsMR allPz1M c
                | any (isWin goal) allPz1M = out vstd allPz1M
--                | any (isWin goal) allPz1M = outAll vstd allPz1M
                | otherwise = goM vstd pzsMR allPz1M c
            goD vstd blkL [] allPz1M c = goD vstd (tail blkL) [pz] allPz1M c
            goD vstd blkL pzsD allPz1M c  = goD visitedD blkL pzs'' (allPz1M ++ pzs'') c
                where
                blk = head blkL
                isTarget = blk == 0
                (visitedD, pzs'') = newPositionsD pz blk rlc vstd pzsD

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
        winPz pzss = filter (isWin goal) pzss
        -- array list of puzzles from start to winning puzzle
        getPathL vstd pzss = go [pzss] where
            go pathL
                | parent == root = pathL 
                | otherwise = go (parent : pathL) where
                (pz, parent) = vstd M.! (reduce (head pathL))
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

testcase12 = A.array (0,9) [(0,((0,1),[(0,1),(0,2),(1,1),(1,2)])),(1,((0,0),[(0,0),(1,0)])),(2,((0,3),[(0,3),(1,3)])),(3,((2,0),[(2,0)])),(4,((2,1),[(2,1),(2,2)])),(5,((2,3),[(2,3)])),(6,((3,0),[(3,0)])),(7,((3,1),[(3,1),(3,2)])),(8,((3,3),[(3,3)])),(9,((4,1),[(4,1),(4,2)]))]

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

--analyse best DSF block order
solBlkOrd = [12::Int,10,6,2,0,4,5,9,10,6,2,0,4,5,9,0,2,10,8,0,9,1,3,7,11,0,6,0]
blkIL = [0..12::Int]
cntBlkL = zip [length $ ocF solBlkOrd | ocF <- map elemIndices blkIL] blkIL
ordBlkL = map snd $ reverse $ sort cntBlkL
--assumption: best to sort by size of block ascending and then 
--sort them in shortest geometrical distance (sortBy position)
v=4::Int;h=3::Int
geoSort p = sortBy (\(_,((y1,x1),_)) (_,((y2,x2),_)) 
    -> compare ((v-y1)^2+(h-x1)^2) ((v-y2)^2+(h-x2)^2)) p
lenSort x = sortBy (\(_,(_,list1)) (_,(_,list2)) 
    -> compare (length list1) (length list2)) x
geoLenSort p = lenSort $ geoSort $ (A.assocs p)
geoSortIdxL p = map (\(idx,(pos,posL)) -> idx) $ geoLenSort p

rotate x = (tail x) ++ [(head x)]

allBlkPL_NotIn :: Eq a => [a] -> [a] -> Bool
allBlkPL_NotIn a b = all (== True) [all (/=x) b | x <- a]

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
103
9 J (4,1) (4,0)
8 I (3,3) (4,2) [9,8,5,2,0,4,7,6,3,1]
5 F (2,3) (4,3)
2 C (0,3) (2,3)
0 B (0,1) (0,2)
1 A (0,0) (0,1)
3 D (2,0) (0,0)
6 G (3,0) (1,0)
7 H (3,1) (3,0)
4 E (2,1) (2,0)
2 C (2,3) (2,2)
3 F (4,3) (2,3)
8 I (4,2) (3,3)
9 J (4,0) (4,2)
7 H (3,0) (4,0)
4 E (2,0) (3,0)
6 G (1,0) (2,1)
3 D (0,0) (2,0)
1 A (0,1) (0,0)
0 B (0,2) (0,1)
5 F (2,3) (0,3)
8 I (3,3) (1,3)
2 C (2,2) (2,3)
4 E (3,0) (3,1)
3 D (2,0) (3,0)
6 G (2,1) (2,0)
4 E (3,1) (2,1)
3 D (3,0) (3,2)
7 H (4,0) (3,0)
9 J (4,2) (4,0)
3 D (3,2) (4,2)
7 H (3,0) (3,1)
2 C (2,3) (3,3)
4 E (2,1) (2,2)
G (2,0) (2,1)
A (0,0) (2,0)
B (0,1) (0,0)
I (1,3) (0,2)
E (2,2) (1,2)
G (2,1) (2,2)
C (3,3) (2,3)
D (4,2) (4,3)
J (4,0) (4,1)
A (2,0) (3,0)
B (0,0) (1,0)
I (0,2) (0,0)
F (0,3) (0,1)
E (1,2) (0,2)
G (2,2) (1,3)
B (1,0) (1,1)
I (0,0) (2,0)
F (0,1) (1,0)
E (0,2) (0,0)
G (1,3) (0,2)
C (2,3) (0,3)
D (4,3) (2,3)
J (4,1) (4,2)
H (3,1) (3,2)
A (3,0) (3,1)
I (2,0) (4,0)
F (1,0) (3,0)
B (1,1) (1,0)
G (0,2) (1,2)
E (0,0) (0,1)
D (2,3) (2,2)
C (0,3) (1,3)
E (0,1) (0,2)
B (1,0) (0,0)
F (3,0) (2,0)
I (4,0) (3,0)
A (3,1) (2,1)
J (4,2) (4,0)
H (3,2) (4,2)
C (1,3) (2,3)
G (1,2) (1,3)
D (2,2) (1,2)
A (2,1) (2,2)
F (2,0) (3,1)
B (0,0) (1,0)
E (0,2) (0,0)
G (1,3) (0,3)
D (1,2) (0,2)
C (2,3) (1,3)
A (2,2) (1,2)
H (4,2) (3,2)
J (4,0) (4,2)
I (3,0) (4,0)
F (3,1) (4,1)
B (1,0) (2,0)
E (0,0) (1,0)
D (0,2) (0,0)
G (0,3) (0,1)
C (1,3) (0,3)
A (1,2) (0,2)
H (3,2) (2,2)
J (4,2) (3,2)
F (4,1) (4,3)
I (4,0) (4,2)
B (2,0) (3,0)
H (2,2) (2,0)
J (3,2) (2,2)
I (4,2) (3,3)
B (3,0) (3,1)


testcase 8:
5 4
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

my output: (after ordering DFS blocks processing)
28
12 BB (4,3) (4,1)
10 KW (3,3) (4,2)
6 LS (2,3) (4,3)
2 RS (0,3) (2,3)
0 BS (0,1) (0,2)
4 TS (2,1) (0,1)
5 LW (2,2) (1,1)
9 DS (3,2) (2,1)
10 KW (4,2) (3,2)
6 LS (4,3) (4,2)
2 RS (2,3) (3,3)
0 BS (0,2) (1,2)
4 TS (0,1) (0,3)
5 LW (1,1) (0,2)
9 DS (2,1) (0,1)
0 BS (1,2) (1,1)
2 RS (3,3) (1,3)
10 KW (3,2) (4,3)
8 DW (3,1) (3,3)
0 BS (1,1) (2,1)
9 DS (0,1) (1,2)
1 RW (0,0) (0,1)
3 TW (2,0) (0,0)
7 BW (3,0) (1,0)
11 KS (4,0) (2,0)
0 BB (4,1) (3,0)
6 LS (4,2) (4,0)
0 BS (2,1) (3,1)
analyse best order for blocks:
order of occurance in solution list:
[12,10,6,2,0,4,5,9,10,6,2,0,4,5,9,0,2,10,8,0,9,1,3,7,11,0,6,0]

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
as different puzzles, to shorten run time (can not be solved in hours now)
(46 seconds with last:)
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
19
P (1,1) (1,0)
solution: DFS for each block

17
P H (1,1) (3,0)
D A (0,0) (3,1)
T E (1,2) (0,0)
D A (3,1) (2,3)
T E (0,0) (1,1)
K B (0,1) (0,0)
L C (0,3) (0,2)
S D (0,4) (0,3)
G G (3,2) (3,1)
P H (3,0) (1,0)
B I (4,1) (4,0)
Y J (4,2) (4,1)
M K (4,3) (4,2)
R F (2,4) (3,4)
D A (2,3) (0,5)
R F (3,4) (2,3)
D A (0,5) (5,5)

(49 seconds with last:)
5 4
RB BW BW RW
RB BW BW RW
TW KB DB TB
TW KW DW TB
LB .. .. LW
BW
3 1

28
DW (3,2) (4,1)
DB (2,2) (4,2)
TB (2,3) (2,2)
RW (0,3) (2,3)
BW (0,1) (0,2)
KB (2,1) (0,1)
KW (3,1) (1,1)
DW (4,1) (2,1)
DB (4,2) (3,1)
LW (4,3) (4,1)
RW (2,3) (3,3)
TB (2,2) (3,2)
BW (0,2) (1,2)
KB (0,1) (0,3)
KW (1,1) (0,2)
DW (2,1) (0,1)
BW (1,2) (1,1)
RW (3,3) (1,3)
TB (3,2) (3,3)
DB (3,1) (4,2)
BW (1,1) (2,1)
DW (0,1) (1,2)
RB (0,0) (0,1)
TW (2,0) (0,0)
LB (4,0) (2,0)
LW (4,1) (3,0)
DB (4,2) (4,0)
BW (2,1) (3,1)

BlockArray: array (0,10) [(0,((0,1),[(0,1),(0,2),(1,1),(1,2)])),(1,((0,0),[(0,0),(1,0)])),(2,((0,3),[(0,3),(1,3)])),(3,((2,0),[(2,0),(3,0)])),(4,((2,1),[(2,1)])),(5,((2,2),[(2,2)])),(6,((2,3),[(2,3),(3,3)])),(7,((3,1),[(3,1)])),(8,((3,2),[(3,2)])),(9,((4,0),[(4,0)])),(10,((4,3),[(4,3)]))]
reduced Block List: [[[(0,1),(0,2),(1,1),(1,2)]],[[(2,1)],[(2,2)],[(3,1)],[(3,2)],[(4,0)],[(4,3)]],[[(0,0),(1,0)],[(0,3),(1,3)],[(2,0),(3,0)],[(2,3),(3,3)]]]
now solution is 28,
problems before: intermittant steps of other block (DB) disurbing move of block DW
36
DW (3,2) (4,2) --
DB (2,2) (3,2)  |--     combine
DW (4,2) (4,1) -- |     combine
DB (3,2) (4,2) ----
TB (2,3) (2,2)
...
other problem: corner move done by different blocks (2 moves) should be reduced to no movement of one blk (LW),
while other block (TB) is doing 2 step move (solved in function addPositionM)
TB (3,2) (3,3)
LW (4,1) (4,2)   ---  reduce to DB (3,1) (4,2)   (stay with LW)
DB (3,1) (4,1) 20---
...
-}
{-
[".A..","AB.C","...C"] Target: "C" (0,0) (0,0): '.' Blocks: "ABC"
BlockMap: fromList [('A',((0,0),[(0,1),(1,0)])),('B',((1,1),[(1,1)])),('C',((1,3),[(1,3),(2,3)]))]
BlockArray: array (0,3) [(0,((0,0),[(0,0),(3,4)])),(1,((0,0),[(0,1),(1,0)])),(2,((1,1),[(1,1)])),(3,((1,3),[(1,3),(2,3)]))]
-}
