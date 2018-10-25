module Main where
import Data.List (elemIndices, minimum, (\\), notElem, all, findIndex, sort, repeat)
import qualified Data.Set as Set   -- (insert, member, empty)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Maybe as Maybe

-- Vector addition.
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
    let c = filter (/= ' ') cu
    cs <- getLn (x - 1)
    return (c:cs)

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
createBlockArray :: Ord a => [a] -> [[a]] -> A.Array Int ((Int, Int), [(Int, Int)])
createBlockArray blk pz =   let blM = createBlockMap blk pz
                                getL x = blM M.! x 
                                arrL = zip [1..] (map getL blk)
                                dimensions = (0, ((0,0), [(0,0),(length pz, length $ pz !! 0)]))
                            in A.array (0,length blk) (dimensions : arrL)

-- Is this a winning position?
isWin :: (Eq a, A.Ix i) => i -> a -> A.Array i (a, b) -> Bool
isWin targIdx goal pz = goal == (fst $ pz A.! targIdx)

-- Try to move the blk in in puzzle in direction dir. Return Just the new
-- puzzle, or Nothing if the move isn’t legal.
moveBlk :: (Num i, Num t, Num t1, Ord t, Ord t1, A.Ix i) =>
    A.Array i ((t, t1), [(t, t1)]) -> i -> (t, t1) -> Maybe (A.Array i ((t, t1), [(t, t1)]))
moveBlk pz blk dir
    | check = Just $ pz A.// [(blk, (blockPos, poses2))]
    | otherwise = Nothing where
    check = newNotInOtherBlks && newNotOut
    poses = snd $ pz A.! blk;  poses2 = map (add dir) poses
    new = poses2 \\ poses
    blockPos = (\(y, x) -> (minimum y, minimum x)) $ unzip poses2
    rlc = (snd $ pz A.! 0) !! 1      -- right lower corner
    xMax = snd rlc - 1;  yMax = fst rlc - 1
    blks = tail $ A.indices pz
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
--allMoves :: (Num t, Num t1, Num i, Ord t, Ord t1, A.Ix i) =>
--    [A.Array i ((t, t1), [(t, t1)])] -> Set.Set (A.Array i ((t, t1), [(t, t1)]))
allMoves pzs = uniqFstTup $ concatMap allMoves1 pzs where
    allMoves1 pz = zip (Maybe.catMaybes [moveBlk pz blk dir |
                blk <- tail $ A.indices pz,
                dir <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]) (repeat pz)

-- Given a puzzle, return the list of different
-- puzzles that are reachable from it (father) in exactly one move.
allMoves1_ :: (Num t, Num t1, Num i, Ord t, Ord t1, A.Ix i) =>
     A.Array i ((t, t1), [(t, t1)]) -> [A.Array i ((t, t1), [(t, t1)])]
allMoves1_ pz = Maybe.catMaybes [moveBlk pz blk dir |
                blk <- tail $ A.indices pz,
                dir <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]


{-   breadth first search
1. Define a node to begin search; store it to queue
2. Take a node from beginning of queue.
    If found searched element, end and return "found"
    else add all followers of this node to end of queue
3. If queue is empty, all nodes were visited; end search and return "not found"
4. Repeat from step 2.        
-}

-- Add puzzle p to the visited map with its parent.
-- Return the updated map and Just p if p wasn’t previously visited,
-- Nothing otherwise.
addPosition :: (Ord t, Ord t1) => M.Map t t1 -> (t, t1) -> (M.Map t t1, Maybe t)
addPosition visited (p, parent) = (visited', q)
      where (old_parent, visited') = M.insertLookupWithKey f p parent visited
            f _ new old = min new old
            q = case old_parent of
                    Nothing -> Just p
                    Just _ -> Nothing
addPositions :: (Ord a, Ord t) => M.Map a t -> [(a, t)] -> (M.Map a t, [a])
addPositions visited [] = (visited, [])
addPositions visited ((p, parent):ps) = (visited'', qs)
    where qs = case q of Just p' -> p':ps'
                         Nothing -> ps'
          (visited', ps') = addPositions visited ps
          (visited'', q) = addPosition visited' (p, parent)

-- Given the map of visited puzzles and the list
-- of current puzzles, return an updated map with all next moves
newPositions :: (Num t, Num t1, Num i, Ord t, Ord t1, A.Ix i) =>
     M.Map (A.Array i ((t, t1), [(t, t1)])) (A.Array i ((t, t1), [(t, t1)]))
     -> [A.Array i ((t, t1), [(t, t1)])]
     -> (M.Map (A.Array i ((t, t1), [(t, t1)])) (A.Array i ((t, t1), [(t, t1)])),
         [A.Array i ((t, t1), [(t, t1)])])
newPositions visited curr_pzs = addPositions visited (allMoves curr_pzs)

-- Go level by level (level = all puzzles reachable with 1 step) 
-- through all reachable puzzles from starting puzzle
findPuzzles targIdx goal start = go (M.singleton start root) [start] where
    go visited pzs
        | any (isWin targIdx goal) pzs = getPathL
        | otherwise = go visited' pzs' where
        (visited', pzs') = newPositions visited pzs
        winPz = head $ filter (isWin targIdx goal) pzs
        getPathL = go [winPz] where
            go pathL
                | parent == root = pathL 
                | otherwise = go (parent : pathL) where
                    parent = visited M.! (head pathL)
    root = A.array (0,0) [(0, (start A.! 0) )]

main :: IO ()
main = do
    m_nL <- fmap (map (read :: String -> Int).words) getLine
    let m = m_nL !! 0   -- y (lines)
        n = m_nL !! 1   -- x (columns)
    p <- getLn m   -- puzzle (max 6 x 6)
    let bl = filter (/= '.') $ unique $ concat p   -- blocks f.e. "ABC"  block 'A', 'B', 'C' (max 3 x 3 each)
        blM = createBlockMap bl p
        blA = createBlockArray bl p
    targS <- getLine
    goalL <- fmap (map (read :: String -> Int).words) getLine
    let goal = (goalL !! 0, goalL !! 1)
        targ = head targS
    let Just targIdx = if mBIdx /= Nothing then mBIdx else Just 1  -- make 1 default, if target not existing
        mBIdx = findIndex (== targ) ('P' : bl)  -- in array first position is puzzledimensions
    putStrLn $ show p ++ "  Target: " ++ show targ ++ " " ++ show goal ++ "  (0,0): " ++ show ((p !! 0) !! 0)
        ++ "  Blocks: " ++ show bl ++ "  Targetindex: " ++ show targIdx
    putStrLn $ "isWin: " ++ show (isWin targIdx goal blA)
    putStrLn $ "BlockMap: " ++ show blM
    putStrLn $ "BlockArray: " ++ show blA
    putStrLn $ "all moves from start: " ++ show (allMoves [blA])
    putStrLn $ "found: " ++ show (findPuzzles targIdx goal blA)

{-
*Main> main
3 4
. A . . 
A B . C
. . . C
B
0 0
-}
{-
[".A..","AB.C","...C"] Target: "C" (0,0) (0,0): '.' Blocks: "ABC"
BlockMap: fromList [('A',((0,0),[(0,1),(1,0)])),('B',((1,1),[(1,1)])),('C',((1,3),[(1,3),(2,3)]))]
BlockArray: array (0,3) [(0,((0,0),[(0,0),(3,4)])),(1,((0,0),[(0,1),(1,0)])),(2,((1,1),[(1,1)])),(3,((1,3),[(1,3),(2,3)]))]
-}
