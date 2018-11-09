module Main where
import Data.List (elemIndices, minimum, (\\), notElem, all, sort, repeat, elemIndex)
import qualified Data.Set as Set   -- (insert, member, empty)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Maybe as Maybe

type PArray = A.Array Int ((Int, Int), [(Int, Int)])
type Coord = (Int, Int)
type VMap = M.Map VMapKey (PArray, PArray)
type VMapKey = [[[Coord]]]
type OneBlkA = A.Array Int Bool

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
-- only double moves of same block
allMoves2 :: Coord -> [PArray] -> [(PArray, PArray)]
allMoves2 rlc pzs = uniqFstTup $ concatMap allMoves'' pzs where
    allMoves'' pz = zip ((unique $ concatMap blkDoubMvs (A.indices pz)) \\ [pz]) (repeat pz)
        where
        blkDoubMvs blk = (concatMap (moved blk) (moved blk pz))
    moved blk pz = (Maybe.catMaybes [moveBlk rlc pz blk dir |
              dir <- dirs])
mov rlc pz = (Maybe.catMaybes [moveBlk rlc pz blk dir |
          blk <- A.indices pz,
          dir <- dirs]) where
-- target has blk with index 0
allMovesT rlc pzs = uniqFstTup $ concatMap allMoves1 pzs where
    allMoves1 pz = zip (Maybe.catMaybes [moveBlk rlc pz blk dir |
                blk <- [0],
                dir <- dirs]) (repeat pz)
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
addPosition :: VMap -> (PArray, PArray) -> (VMap, Maybe PArray)
addPosition visited (p, parent) = (visited', q)
    where 
    old_p_parent = M.lookup (reduce p) visited
    (visited', q) = case old_p_parent of
        Nothing -> (M.insert (reduce p) (p, parent) visited, Just p)
        Just _ -> (visited, Nothing)

-- Multiple step optimization / correction:
-- Add puzzle p to the visited map with its parent.
-- if p wasn't previously visited (not in map) and if "optimization"
--     then Do not update map and return Nothing,  
--     else return the updated map and Just p if p wasn’t previously visited
-- Otherwise do not update map and return Nothing. Use addPosition for single steps mode
-- optimization: 2 single steps from 2 blocks can be reduced to single steps of just 1 block 
addPositionM :: OneBlkA -> VMap -> (PArray, PArray) -> (VMap, Maybe PArray)
addPositionM oneBlkA visited (p, parent) = (visited', q)
    where
    old_p_parent = M.lookup (reduce p) visited
    (visited', q) = case old_p_parent of
--        Nothing -> (M.insert (reduce p) (p, parent) visited, Just p)
        Nothing -> if blkBi <= snd (A.bounds p)
            then if isOneBlk blkAi blkBi && sameBlock pSw parentSw grandPa
                then if movedCntBlk parent grandPa < 2 && opti && isParentSw
                        then (M.insert (reduce pSw) (pSw, grandPa) visited, Just pSw)
                        else (M.insert (reduce p) (p, parent) visited, Just p)
                else (M.insert (reduce p) (p, parent) visited, Just p)
            else (M.insert (reduce p) (p, parent) visited, Just p)

        Just _ -> (visited, Nothing)
        -- also correct working, but 15 % slower than solution in Nothing part
--        Just (oldP, oldParent) -> if oBlkBi <= snd (A.bounds p)
--            then if sameBlock oldP oldParent oldGrandPa
--                then (visited, Nothing)
--                else if isOneBlk oBlkAi oBlkBi && sameBlock opSw oparentSw oldGrandPa && oldGrandPa /= p
--                    then if movedCntBlk oldParent oldGrandPa < 2 && oopti && isoParentSw
--                            then (M.insert (reduce opSw) (opSw, oldGrandPa) visited, Just opSw)
--                            else (visited, Nothing)
--                    else (visited, Nothing)
--            else (visited, Nothing)
--                where
--                readoldParentm = M.lookup (reduce oldParent) visited
--                (_, oldGrandPa) = case readoldParentm of
--                    Nothing -> (p, p)
--                    Just (readOP, oldGrandPa) -> (readOP, oldGrandPa)
----                (_, oldGrandPa) = visited M.! (reduce oldParent)
--                oBlkAi = movedBlk oldP oldParent; oBlkBi = movedBlk oldParent oldGrandPa
--                opSw = oldP A.// [(oBlkAi, opBelem), (oBlkBi, opAelem)]
--                oparentSw = oldParent A.// [(oBlkAi, (oparAposSw, oparPosLASw)), (oBlkBi, (oparBposSw, oparPosLBSw))]
--                opAelem = oldP A.! oBlkAi; opBelem = oldP A.! oBlkBi
--                oparAposSw = add oswap oparApos; oparBposSw = sub oswap oparBpos
--                oparApos = fst $ oldParent A.! oBlkAi; oparBpos = fst $ oldParent A.! oBlkBi
--                oparPosLASw = map (add oswap) (snd $ oldParent A.! oBlkAi)
--                oparPosLBSw = map (sub oswap) (snd $ oldParent A.! oBlkBi)
--                oswap = sub opApos opBpos
--                opApos = fst $ oldP A.! oBlkAi; opBpos = fst $ oldP A.! oBlkBi
--                ogPaApos = fst $ oldGrandPa A.! oBlkAi; ogPaBpos = fst $ oldGrandPa A.! oBlkBi
--                oopti
--                    | lenILp > 1 =   --readParent == parent && 
--                        (any (==True) $ map comparison_ dirABlist)
--                    | otherwise = False
--                    where
--                    comparison_ (a, b) = add a opApos == ogPaApos && add b opBpos == ogPaBpos
--                readParentSw = M.lookup (reduce oparentSw) visited
--                (isoParentSw, rgPsw) = case readParentSw of
--                    Nothing -> (False, p)
--                    Just (_, rogPsw) -> (True, rogPsw)
    opti
        | lenILp > 1 =   --readParent == parent && 
            (any (==True) $ map comparison dirABlist)
        | otherwise = False
        where
        comparison (a, b) = add a pApos == gPaApos && add b pBpos == gPaBpos
    dirABlist = [((0,-1), (-1,0)), ((1, 0), (0,-1)), ((0,1), (-1, 0)), ((-1,0), (0,-1)),
                ((0, -1), (1, 0)), ((1, 0), (0, 1)), ((0,1), (1, 0)), ((-1,0), (0,1))]
    isOneBlk blkAi_ blkBi_ = oneBlkA A.! blkAi_ && oneBlkA A.! blkBi_
    sameBlock px par gPar = movedBlk px par == movedBlk par gPar
    lenILp = length (A.indices p)
--    isNotRoot pz = fst (pz A.! 0) /= (-1, 0)
    readParentSw = M.lookup (reduce parentSw) visited
    (isParentSw, rgPsw) = case readParentSw of
        Nothing -> (False, p)
        Just (_, rgPsw) -> (True, rgPsw)
    swap = sub pApos pBpos
--    pAposSw = add swap pApos; pBposSw = sub swap pBpos
    parAposSw = add swap parApos; parBposSw = sub swap parBpos
    pSw = p A.// [(blkAi, pBelem), (blkBi, pAelem)]
    parentSw = parent A.// [(blkAi, (parAposSw, parPosLASw)), (blkBi, (parBposSw, parPosLBSw))]
--    pPosLA = snd $ p A.! blkAi; pPosLB = snd $ p A.! blkBi 
    parPosLASw = map (add swap) (snd $ parent A.! blkAi); parPosLBSw = map (sub swap) (snd $ parent A.! blkBi)
    pAelem = p A.! blkAi; pBelem = p A.! blkBi
    pApos = fst $ p A.! blkAi; pBpos = fst $ p A.! blkBi
    parApos = fst $ parent A.! blkAi; parBpos = fst $ parent A.! blkBi
    gPaApos = fst $ grandPa A.! blkAi; gPaBpos = fst $ grandPa A.! blkBi
    (_, grandPa) = visited M.! (reduce parent) -- parent always existing
    blks = A.indices p
    blkAi = movedBlk p parent; blkBi = movedBlk parent grandPa
----    root_ = A.array (A.bounds p) [(blki,((-1::Int,0::Int),[(0::Int,0::Int)])) | blki <- A.indices p]
    movedBlk p1 p2 = sum $ map map_f blks
        where
        map_f blk = if fil blk then blk else 0
        fil bl = p1 A.! bl /= p2 A.! bl
    movedCntBlk p1 p2 = sum $ map map_f blks
        where
        map_f blk = if fil blk then 1 else 0
        fil bl = p1 A.! bl /= p2 A.! bl

-- just reducing multiple steps to 1 move -- blkMovesL is much faster on end, doing this job - and correct
-- leaving ABAB instead of AB
addPosition3 :: VMap -> (PArray, PArray) -> (VMap, Maybe PArray)
addPosition3 visited (p, parent) = (visited', q)
    where
    old_p_parent = M.lookup (reduce p) visited
    (visited', q) = case old_p_parent of
        Nothing -> if sameBlock p parent grandPar
            then ins grandPar (Just p)
            else ins parent (Just p)
        Just (oldP, oldParent) -> (visited, Nothing)
        where
        ins par mB = (M.insert (reduce p) (p, par) visited, mB)
        (readParent, grandPar) = visited M.! (reduce parent) -- parent always existing
        blks = A.indices p
        sameBlock px par gPar = movedBlk px par == movedBlk par gPar
        movedBlk p1 p2 = sum $ map map_f blks
        --movedBlk p1 p2 = head $ filter fil blks
    	    where
            map_f blk = if fil blk then blk else 0
            fil bl = p1 A.! bl /= p2 A.! bl
-- take visited map and list of puzzles with each parent to store to visited map
-- return updated map and new puzzles (not previously already in visited map)
-- for processing next level
addPositions :: VMap -> [(PArray, PArray)] -> (VMap, [PArray])
addPositions visited [] = (visited, [])
addPositions visited ((p, parent):ps) = (visited'', qs)
    where qs = case q of Just p' -> p':ps'
                         Nothing -> ps'
          (visited', ps') = addPositions visited ps
          (visited'', q) = addPosition visited' (p, parent)
addPositionsM :: OneBlkA -> VMap -> [(PArray, PArray)] -> (VMap, [PArray])
addPositionsM oneBlkA visited [] = (visited, [])
addPositionsM oneBlkA visited ((p, parent):ps) = (visited'', qs)
    where qs = case q of Just p' -> p':ps'
                         Nothing -> ps'
          (visited', ps') = addPositionsM oneBlkA visited ps
          (visited'', q) = addPositionM oneBlkA visited' (p, parent)

-- Given the map of visited puzzles and the list
-- of current puzzles, return an updated map with all next moves
newPositions :: Coord -> VMap -> [PArray] -> (VMap,[PArray])
newPositions rlc visited curr_pzs = addPositions visited (allMoves rlc curr_pzs)
newPositionsM oneBlkA rlc visited curr_pzs = addPositionsM oneBlkA visited (allMoves rlc curr_pzs)

newPositionsT rlc visited curr_pzs = addPositions visited (allMovesT rlc curr_pzs)

-- Go level by level (level = all puzzles reachable with 1 step) 
-- through all reachable puzzles from starting puzzle
findPuzzles :: OneBlkA -> Coord -> Coord -> [[Char]] -> PArray -> [Char]
findPuzzles oneBlkA rlc goal blkL start = go (M.singleton (reduce start) (start, root)) [start] where
    go visited pzs
        | any (isWin goal) pzs = cvtToOut (blkMovesL visited (minimum $ winPz pzs))
--        | any (isWin goal) pzs = concatMap cvt_bML (winPz pzs)
        | otherwise = srchTgtWin where   --go visited' pzs'
        -- can target already reach goal?
        cvt_bML x = cvtToOut (blkMovesL visited x)
        srchTgtWin = goT visited pzs
            where
            goT _ [] = go visited' pzs'  -- go1 visited' pzs'  -- with 2 steps / 2 step move
            goT vstd pzsT
                | any (isWin goal) pzsT = cvtToOut (blkMovesL vstd (minimum $ winPz pzsT))
--                | any (isWin goal) pzsT = concatMap cvt_bMLT (winPz pzsT)
                | otherwise = goT visitedT pzs''
                where
                cvt_bMLT x = cvtToOut (blkMovesL vstd x)
                (visitedT, pzs'') = newPositionsT rlc vstd pzsT
--        (visited', pzs') = newPositions rlc visited pzs         -- for single steps
        (visited', pzs') = newPositionsM oneBlkA rlc visited pzs  -- for multiple steps move
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
        -- change to list of blocks with their entire coherently moves (summing single step moves)
        blkMovesL vstd pzss = go (blkStepsL vstd pzss) [] ("x") (0, 0) where
            go [] blkCML _ _ = blkCML
            go blkML@(blkS@(blk, from, to) : restBML) blkCML prevBlk saveFrom
                | t5Blk /= [] && chkt5Blk = go (chgd5Blk ++ restOf5BML) blkCML prevBlk saveFrom
                | t3Blk /= [] && chkt3Blk = go (chgd3Blk ++ restOf3BML) blkCML prevBlk saveFrom
                | blk /= prevBlk && nextBlk /= blk = go restBML (blkCML ++ [blkS]) blk from
                | blk /= prevBlk && nextBlk == blk = go restBML blkCML blk from
                | blk == prevBlk && nextBlk == blk = go restBML blkCML blk saveFrom
                | otherwise = go restBML (blkCML ++ [(blk, saveFrom, to)]) blk from
                where
                t5Blk
                    | (length $ take 5 blkML) > 4 = take 5 blkML
                    | otherwise = []
                t3Blk
                    | (length $ take 3 blkML) > 2 = take 3 blkML
                    | otherwise = []
                restOf5BML = drop 5 blkML
                restOf3BML = drop 3 blkML
                chkt5Blk = n51 == n4 && n52 == n5 && n1 /= n2 && n1 /= n3 && blks5NotOverlap
                -- and all blkPL of n4 "to" not in n2 "from" and "to" blkPL and
                                          --not in n3 "from" and "to" blkPL and
                    -- all blkPL of n5 "to" not in n3 "from" and "to" blkPL
                blks5NotOverlap = allBlkPL_NotIn (nXYblkPL n4 t4) (nXYblkPL n52 f52) && 
                                allBlkPL_NotIn (nXYblkPL n4 t4) (nXYblkPL n52 t52) &&
                                allBlkPL_NotIn (nXYblkPL n4 t4) (nXYblkPL n53 f53) &&
                                allBlkPL_NotIn (nXYblkPL n4 t4) (nXYblkPL n53 t53) &&
                                allBlkPL_NotIn (nXYblkPL n5 t5) (nXYblkPL n53 f53) &&
                                allBlkPL_NotIn (nXYblkPL n5 t5) (nXYblkPL n53 t53)
                chkt3Blk = n1 == n3 && n1 /= n2 && blks3NotOverlap
                -- and all blkPL of n3 "to" not in n2 "from" and "to" blkPL
                blks3NotOverlap = allBlkPL_NotIn (nXYblkPL n3 t3) (nXYblkPL n2 f2) && 
                                allBlkPL_NotIn (nXYblkPL n3 t3) (nXYblkPL n2 t2)
                nXYblkPL x y = map (add y) (nXblkPL x) 
                nXblkPL x = map (sub pos) blkPL
                    where
                    (pos, blkPL) = blk'' (blkIdx x) 
                chgd5Blk = b51 : b4 : b52 : b5 : [b53]
                chgd3Blk = b1 : b3 : [b2]
                (b51@(n51,_,_) : b52@(n52,f52,t52) : b53@(n53,f53,t53) : b4@(n4,_,t4) : [b5@(n5,_,t5)]) = t5Blk
                (b1@(n1,_,_) : b2@(n2,f2,t2) : [b3@(n3,_,t3)]) = t3Blk
                blkIdx blk_ = snd $ head $ filter (\(x,y) -> x == blk_) $ zip blkL [0..] 
                blk'' blki = (head pzs) A.! blki
                nextBlk = if restBML /= [] then extractNext restBML else "n" where
                    extractNext ((next, _, _) : _) = next
                red2to1 = undefined
{- example pattern:  t3Blk reduction
DW (3,2) (4,2)      DW (3,2) (4,2)
DB (2,2) (3,2) - -> DW (4,2) (4,1)  -->  Dw (3,2) (4,1)
                X
DW (4,2) (4,1) - -> DB (2,2) (3,2)

DB (3,2) (4,2)      DB (3,2) (4,2)  -->  DB (2,2) (4,2)
-}
        cvtToOut blkmvL = show len ++ "\n" ++ (unlines $ map str blkmvL) where
            len = length blkmvL
            str (x,y,z) = x ++ " " ++ show y ++ " " ++ show z
    root = A.array (A.bounds start) [(blki,((-1::Int,0::Int),[(0::Int,0::Int)])) | blki <- A.indices start]
-- blkStepsL:
-- [("B",(1,1),(1,2)),("A",(0,0),(1,0)),("B",(1,2),(0,2)),("B",(0,2),(0,1)),("B",(0,1),(0,0))]

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
        oneBlkA = A.array (A.bounds blA) (map isOneBlk (A.assocs blA))
        goal = (goalL !! 0, goalL !! 1)
    putStr $ findPuzzles oneBlkA (m, n) goal bl blA
--    putStr ""
{-
    putStrLn $ "Block Array: " ++ show blA
    putStrLn $ "OneBlock Array: " ++ show oneBlkA
    putStrLn $ "reduced Block List: " ++ show (reduce blA)
    putStrLn $ show p ++ "  Target: " ++ show targS ++ " " ++ show goal ++ "  (0,0): " ++ show ((p !! 0) !! 0)
        ++ "  Blocks: " ++ show bl ++ "  Targetindex: " ++ show targIdx ++ " changed to 0"
    putStrLn $ "BlockMap: " ++ show blM
    putStrLn $ show (allMoves1_ (m, n) blA)
    putStrLn $ "isWin: " ++ show (isWin goal blA)
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
Output:
D (2,0) (0,5)

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

(49 seconds with last:)
5 4
A B B C
A B B C
D 1 2 E
D 3 4 E
F . . G
B
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

mein output:

5 4
RB BW BW RW
RB BW BW RW
TW KB DB TB
TW KW DW TB
LB .. .. LW
BW
3 1
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
