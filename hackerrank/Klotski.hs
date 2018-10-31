module Main where
import Data.List (elemIndices, minimum, (\\), notElem, all, sort, repeat, elemIndex)
import qualified Data.Set as Set   -- (insert, member, empty)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Maybe as Maybe

-- Vector addition.
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
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
createBlockArray :: Ord a => [[a]] -> [[[a]]] -> A.Array Int ((Int, Int), [(Int, Int)])
createBlockArray blk pz =   let blM = createBlockMap blk pz
                                getL x = blM M.! x 
                                arrL = zip [0..] (map getL blk)
                            in A.array (0,length blk - 1) arrL

-- Is this a winning position?
isWin :: (Eq a, A.Ix i) => i -> a -> A.Array i (a, b) -> Bool
isWin targIdx goal pz = goal == (fst $ pz A.! targIdx)

-- Try to move the blk in in puzzle in direction dir. Return Just the new
-- puzzle, or Nothing if the move isn’t legal.
moveBlk :: (Num t, Num t1, Ord t, Ord t1, A.Ix i) =>
    (t, t1) -> A.Array i ((t, t1), [(t, t1)]) -> i -> (t, t1) -> Maybe (A.Array i ((t, t1), [(t, t1)]))
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
allMoves :: (Num t, Num t1, Ord t, Ord t1, A.Ix i) =>
    (t, t1) -> [A.Array i ((t, t1), [(t, t1)])]
     -> [(A.Array i ((t, t1), [(t, t1)]), A.Array i ((t, t1), [(t, t1)]))]
allMoves rlc pzs = uniqFstTup $ concatMap allMoves1 pzs where
    allMoves1 pz = zip (Maybe.catMaybes [moveBlk rlc pz blk dir |
                blk <- A.indices pz,
                dir <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]) (repeat pz)

allMovesT rlc targIdx pzs = uniqFstTup $ concatMap allMoves1 pzs where
    allMoves1 pz = zip (Maybe.catMaybes [moveBlk rlc pz blk dir |
                blk <- [targIdx],
                dir <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]) (repeat pz)

-- not used - just for testing
-- Given a puzzle, return the list of different
-- puzzles that are reachable from it (father) in exactly one move.
allMoves1_ :: (Num t, Num t1, Ord t, Ord t1, A.Ix i) =>
     (t, t1) -> A.Array i ((t, t1), [(t, t1)]) -> [A.Array i ((t, t1), [(t, t1)])]
allMoves1_ rlc pz = Maybe.catMaybes [moveBlk rlc pz blk dir |
                blk <- A.indices pz,
                dir <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]

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
reduce :: (Enum a, Num t, Num t1, Num a, Ord t, Ord t1, Ord a, A.Ix i) =>
     A.Array i ((t, t1), [(t, t1)]) -> [(a, [[(t, t1)]])]
reduce pz = go blsL M.empty where
    go [] blM            = M.toList blM
    go (x@(pos, eL):xs) blM = go xs blSM
        where
        i = redBlM M.! blM1To0
        blM1To0 = map (sub pos) eL
        mby = M.lookup i blM
        blSM = case mby of
            Just old -> M.insert i (sort (old ++ [eL])) blM
            Nothing -> M.insert i ([eL]) blM
--    blsML = A.assocs pz
    blsL = A.elems pz
    blMTo0 blL = (\(p,l) -> map (sub p) l) blL
    redBlSet = Set.fromList $ map blMTo0 blsL
    redBlL = Set.toList redBlSet
    redBlM = M.fromList $ zip redBlL [0..]

-- Add puzzle p to the visited map with its parent.
-- Return the updated map and Just p if p wasn’t previously visited,
-- Nothing otherwise.
addPosition :: (Enum a, Num t1, Num t2, Num a, Ord a, Ord t1, Ord t2,
    A.Ix i) => M.Map [(a, [[(t1, t2)]])] (A.Array i ((t1, t2), [(t1, t2)]), t)
    -> (A.Array i ((t1, t2), [(t1, t2)]), t)
    -> (M.Map [(a, [[(t1, t2)]])] (A.Array i ((t1, t2), [(t1, t2)]), t),
         Maybe (A.Array i ((t1, t2), [(t1, t2)])))
addPosition visited (p, parent) = (visited', q)
      where old_p_parent = M.lookup (reduce p) visited
            (visited', q) = case old_p_parent of
                Nothing -> (M.insert (reduce p) (p, parent) visited, Just p)
                Just _ -> (visited, Nothing)
-- take visited map and list of puzzles with each parent to store to visited map
-- return updated map and new puzzles (not previously already in visited map)
-- for processing next level
addPositions :: (Enum a, Num t1, Num t2, Num a, Ord a, Ord t1, Ord t2,
    A.Ix i) => M.Map [(a, [[(t1, t2)]])] (A.Array i ((t1, t2), [(t1, t2)]), t)
    -> [(A.Array i ((t1, t2), [(t1, t2)]), t)]
    -> (M.Map [(a, [[(t1, t2)]])] (A.Array i ((t1, t2), [(t1, t2)]), t),
         [A.Array i ((t1, t2), [(t1, t2)])])
addPositions visited [] = (visited, [])
addPositions visited ((p, parent):ps) = (visited'', qs)
    where qs = case q of Just p' -> p':ps'
                         Nothing -> ps'
          (visited', ps') = addPositions visited ps
          (visited'', q) = addPosition visited' (p, parent)

-- Given the map of visited puzzles and the list
-- of current puzzles, return an updated map with all next moves
newPositions :: (Enum a, Num t, Num t1, Num a, Ord a, Ord t, Ord t1, A.Ix i) =>
     (t, t1) -> M.Map [(a, [[(t, t1)]])] (A.Array i ((t, t1), [(t, t1)]), A.Array i ((t, t1), [(t, t1)]))
     -> [A.Array i ((t, t1), [(t, t1)])] -> (M.Map [(a, [[(t, t1)]])]
           (A.Array i ((t, t1), [(t, t1)]), A.Array i ((t, t1), [(t, t1)])),
         [A.Array i ((t, t1), [(t, t1)])])
newPositions rlc visited curr_pzs = addPositions visited (allMoves rlc curr_pzs)

newPositionsT rlc targIdx visited curr_pzs = addPositions visited (allMovesT rlc targIdx curr_pzs)

-- Go level by level (level = all puzzles reachable with 1 step) 
-- through all reachable puzzles from starting puzzle
findPuzzles :: (Num t, Num t1, Ord t, Ord t1, Show t, Show t1) =>
     (t, t1) -> Int -> (t, t1) -> [[Char]] -> A.Array Int ((t, t1), [(t, t1)]) -> [Char]
findPuzzles rlc targIdx goal blkL start = go (M.singleton (reduce start) (start, root)) [start] where
    go visited pzs
        | any (isWin targIdx goal) pzs = cvtToOut (blkCMovesL visited pzs)
        | otherwise = srchTgtWin where   --go visited' pzs'
        -- can target already reach goal
        srchTgtWin = goT visited pzs
            where
            goT _ [] = go visited' pzs'
            goT vstd pzsT
                | any (isWin targIdx goal) pzsT = cvtToOut (blkCMovesL vstd pzsT)
                | otherwise = goT visitedT pzs''
                where
                (visitedT, pzs'') = newPositionsT rlc targIdx vstd pzsT
        (visited', pzs') = newPositions rlc visited pzs
        winPz pzss = head $ filter (isWin targIdx goal) pzss
        -- array list of puzzles from start to winning puzzle
        getPathL vstd pzss = go [winPz pzss] where
            go pathL
                | parent == root = pathL 
                | otherwise = go (parent : pathL) where
                (pz, parent) = vstd M.! (reduce (head pathL))
        -- create list of blocks and their single step moves by finding 
        -- the different blocks of neighboring puzzles of solution path
        blkMovesL vstd pzss = map blkMove blkLNeighborsL where
            pzL = map A.assocs (getPathL vstd pzss)
            blkLNeighborsL = zip pzL (tail pzL)
            blkMove (l1, l2) = head $ map cvtToBlkMvs $ filter diffItems $ zip l1 l2 where
                cvtToBlkMvs ((i, blk), (i1, blk1)) = ((blkL !! i), (fst blk), fst blk1)
                diffItems (x, y) = x /= y
        -- change to list of blocks with their entire coherently moves (summing single step moves)
        blkCMovesL vstd pzss = go (blkMovesL vstd pzss) [] ("x") (0, 0) where
            go [] blkCML _ _ = blkCML
            go (blkS @ (blk, from, to) : restBML) blkCML prevBlk saveFrom
                | blk /= prevBlk && nextBlk /= blk = go restBML (blkCML ++ [blkS]) blk from
                | blk /= prevBlk && nextBlk == blk = go restBML blkCML blk from
                | blk == prevBlk && nextBlk == blk = go restBML blkCML blk saveFrom
                | otherwise = go restBML (blkCML ++ [(blk, saveFrom, to)]) blk from
                where
                nextBlk = if restBML /= [] then extractNext restBML else "n" where
                    extractNext ((next, _, _) : _) = next
        cvtToOut blkmvL = show len ++ "\n" ++ (unlines $ map str blkmvL) where
            len = length blkmvL
            str (x,y,z) = x ++ " " ++ show y ++ " " ++ show z
    root = A.array (0,0) [(0, ((0,0), [(0,0),rlc]) )]
-- blkMovesL:
-- [("B",(1,1),(1,2)),("A",(0,0),(1,0)),("B",(1,2),(0,2)),("B",(0,2),(0,1)),("B",(0,1),(0,0))]
-- unlines $ map (\(x,y,z) -> (x:[]) ++ " " ++ show y ++ " " ++ show z) [("B",(1,1),(1,2)),("A",(0,0),(1,0)),("B",(1,2),(0,2)),("B",(0,2),(0,1)),("B",(0,1),(0,0))]
main :: IO ()
main = do
    m_nL <- fmap (map (read :: String -> Int).words) getLine
    let m = m_nL !! 0   -- y (lines)
        n = m_nL !! 1   -- x (columns)
    p <- getLn m   -- puzzle (max 6 x 6)
        -- blocks f.e. ["A", "B", "C"] (max 3 x 3 each)
    let bl = unique $ concat $ map (words . filter (/= '.')) p
        --bl = unique $ concat $ map (filter (/= "") . map (filter (/= '.'))) pz
        pz = map words p
        blM = createBlockMap bl pz
        blA = createBlockArray bl pz
    targS <- getLine
    goalL <- fmap (map (read :: String -> Int).words) getLine
    let goal = (goalL !! 0, goalL !! 1)
    let Just targIdx = if mBIdx /= Nothing then mBIdx else Just 0  -- make 0 default, if target not existing
        mBIdx = elemIndex targS bl 
    putStr $ findPuzzles (m, n) targIdx goal bl blA
--    putStr ""
{-
    putStrLn $ "BlockArray: " ++ show blA
    putStrLn $ "reduced Block List: " ++ show (reduce blA)
    putStrLn $ show (allMoves1_ (m, n) blA)
    putStrLn $ show p ++ "  Target: " ++ show targS ++ " " ++ show goal ++ "  (0,0): " ++ show ((p !! 0) !! 0)
        ++ "  Blocks: " ++ show bl ++ "  Targetindex: " ++ show targIdx
    putStrLn $ "isWin: " ++ show (isWin targIdx goal blA)
    putStrLn $ "BlockMap: " ++ show blM
    putStrLn $ "all moves from start: " ++ show (allMoves (m, n) [blA])
-}
{-
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

needs to anonymize f.e. A and C, D, F, which are equal (2 vertical) not to be differentiated
as different puzzles, to shorten run time (can not be solved in hours now)
5 4
A B B C
A B B C
D E E F
D G H F
I . . J
B
3 1

BlockArray: array (0,9) [(0,((0,0),[(0,0),(1,0)])),(1,((0,1),[(0,1),(0,2),(1,1),(1,2)])),(2,((0,3),[(0,3),(1,3)])),(3,((2,0),[(2,0),(3,0)])),(4,((2,1),[(2,1),(2,2)])),(5,((2,3),[(2,3),(3,3)])),(6,((3,1),[(3,1)])),(7,((3,2),[(3,2)])),(8,((4,0),[(4,0)])),(9,((4,3),[(4,3)]))]
-}
{-
[".A..","AB.C","...C"] Target: "C" (0,0) (0,0): '.' Blocks: "ABC"
BlockMap: fromList [('A',((0,0),[(0,1),(1,0)])),('B',((1,1),[(1,1)])),('C',((1,3),[(1,3),(2,3)]))]
BlockArray: array (0,3) [(0,((0,0),[(0,0),(3,4)])),(1,((0,0),[(0,1),(1,0)])),(2,((1,1),[(1,1)])),(3,((1,3),[(1,3),(2,3)]))]
-}
