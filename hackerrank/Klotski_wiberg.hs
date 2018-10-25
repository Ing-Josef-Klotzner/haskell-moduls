import qualified Data.Array as A
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
-- The contents of a square on the game board.
data Square = Empty | Single | TwoTop | TwoBottom | TwoLeft
    | TwoRight | FourTL | FourTR | FourBL | FourBR
    deriving (Eq, Ord, Show)
-- Spaces occupied by a game piece, given its upper-left  corner,
-- relative to its upper-left corner.
whole Single = [(0, 0)]
whole TwoTop = [(0, 0), (0, 1)]
whole TwoLeft = [(0, 0), (1, 0)]
whole FourTL = [(0, 0), (1, 0), (0, 1), (1, 1)]
whole _ = []
-- Vector addition.
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
-- Initial game board positions.
mkStartPos :: [Square] -> A.Array (Int, Int) Square
mkStartPos state = A.array ((0, 0), (3, 4)) $ zip (foldr (++) [] rows) state
    where rows = [[(x, y) | x <- [0..3]] | y <- [0..4]]
startPos :: A.Array (Int, Int) Square
startPos = mkStartPos [TwoTop, FourTL, FourTR, TwoTop,
    TwoBottom, FourBL, FourBR, TwoBottom,
    TwoTop, TwoLeft, TwoRight, TwoTop,
    TwoBottom, Single, Single, TwoBottom,
    Single, Empty, Empty, Single]
-- Is this a winning position?
isWin :: (Num t, Num t1, A.Ix t, A.Ix t1) => A.Array (t, t1) Square -> Bool
isWin p = (p A.! (1, 3)) == FourTL
-- Is the given position on the game board, and empty?
isEmpty :: A.Ix i => A.Array i Square -> i -> Bool
isEmpty p xy = (get p xy) == Just Empty
    where get p xy = if A.inRange (A.bounds p) xy 
                    then (Just $ p A.! xy) else Nothing
-- Try to move the piece at pos in direction dir. Return Just the new
-- position, or Nothing if the move isn’t legal.
moveSquare :: (Num t, Num t1, A.Ix t, A.Ix t1) => A.Array (t, t1) Square
    -> (t, t1) -> (t, t1) -> Maybe (A.Array (t, t1) Square)
moveSquare p pos dir =
    let poses = map (add pos) (whole $ p A.! pos)
        poses2 = map (add dir) poses
        new = poses2 List.\\ poses
        old = poses List.\\ poses2
    in if new /= [] && all (isEmpty p) new
        then Just $ p A.// ([(add pos dir, p A.! pos) | pos <- poses]
                        ++ [(pos, Empty) | pos <- old])
        else Nothing
-- Given a list of positions, return the list of different
-- positions that are reachable from them in exactly one move.
allMoves :: (Num t, Num t1, A.Ix t, A.Ix t1) =>
    [A.Array (t, t1) Square] -> Set.Set (A.Array (t, t1) Square)
allMoves ps = Set.fromList $ concatMap allMoves1 ps where
    allMoves1 p = Maybe.catMaybes [moveSquare p pos dir |
                pos <- A.indices p,
                dir <- [(0, -1), (0, 1), (-1, 0), (1, 0)]]
-- Add position p to the visited map with the given number of steps.
-- Return the updated map and Just p if p wasn’t previously visited,
-- Nothing otherwise.
addPosition :: (Ord k, Ord a) => (Map.Map k a) -> a -> k -> (Map.Map k a, Maybe k)
addPosition visited steps p = (visited', q)
      where (old_steps, visited') = Map.insertLookupWithKey f p steps visited
            f _ new old = min new old
            q = case old_steps of
                    Nothing -> Just p
                    Just _ -> Nothing
addPositions :: (Ord k, Ord a) => (Map.Map k a) -> a -> [k] -> (Map.Map k a, [k])
addPositions visited steps [] = (visited, [])
addPositions visited steps (p:ps) = (visited'', qs)
    where qs = case q of Just p' -> p':ps'
                         Nothing -> ps'
          (visited', ps') = addPositions visited steps ps
          (visited'', q) = addPosition visited' steps p
-- Given the map of visited positions, the current step, and the list
-- of current positions, return an updated map and list of current
-- positions.
newPositions :: (Num t, Num t1, Ord a, A.Ix t, A.Ix t1) =>
    Map.Map (A.Array (t, t1) Square) a -> a -> [A.Array (t, t1) Square]
    -> (Map.Map (A.Array (t, t1) Square) a, [A.Array (t, t1) Square])
newPositions visited steps old_pos =
    addPositions visited steps (Set.toList $ allMoves old_pos)
-- A list of all reachable positions: at list index k is a list of all
-- positions reachable in exactly k steps.
listPositions :: (Num t, Num t1, A.Ix t, A.Ix t1) =>
    A.Array (t, t1) Square -> [[A.Array (t, t1) Square]]
listPositions start = go (Map.singleton start 0) 1 [start]
    where go visited steps pos = pos : go visited' (steps + 1) pos'
            where (visited', pos') = newPositions visited steps pos
-- Given a list of reachable positions (such as produced by
-- listPositions), return the number of steps to one of the first
-- winning positions, and the position itself.
firstWin :: (Enum t, Num t1, Num t2, Num t, A.Ix t1, A.Ix t2) =>
    [[A.Array (t1, t2) Square]] -> Maybe (t, [A.Array (t1, t2) Square])
firstWin posList = maybe Nothing (\(i, ps) -> Just (i, filter isWin ps))
                    $ List.find winner $ zip [0..] posList
    where winner (i, ps) = any isWin ps
-- Given a position p and a list of candidate positions, return one of
-- the candidates that is also a legal move.
backtrack :: (Num t, Num t1, A.Ix t, A.Ix t1) => A.Array (t, t1) Square
    -> [A.Array (t, t1) Square] -> A.Array (t, t1) Square
backtrack p candidates = head $ Set.toList (Set.intersection (allMoves [p])
                                (Set.fromList candidates))
backtrackAll :: (Num t, Num t1, A.Ix t, A.Ix t1) => A.Array (t, t1) Square
    -> [[A.Array (t, t1) Square]] -> [A.Array (t, t1) Square]
backtrackAll p [] = []
backtrackAll p (c:cs) = p' : backtrackAll p' cs
    where p' = backtrack p c
-- Given a starting position, return a list of positions that goes
-- from the starting position to one of the closest winning positions,
-- one move at a time.
winSequence :: (Num t, Num t1, A.Ix t, A.Ix t1) =>
    A.Array (t, t1) Square -> [A.Array (t, t1) Square]
winSequence start = let posList = listPositions start
                        Just (winStep, winPositions) = firstWin posList
                        winPos = head winPositions
                        revPosList = reverse $ take winStep posList
                        revWinSeq = winPos : backtrackAll winPos revPosList
                    in reverse revWinSeq
-- *Main> length $ concat $ take 116 (listPositions startPos)
--23975    ... number of positions until level 116

-- Return a list of strings. Each string is one line in a LaTeX
-- fragment that draws the given position.
drawPos :: (Integral a1, Integral a2, Show a, A.Ix a1, A.Ix a2) =>
    A.Array (a1, a2) Square -> a -> [[Char]]
drawPos p steps = ["\\subsection*{After " ++ (show steps) ++ " steps}",
                    "\\begin{center}",
                    "\\setlength{\\unitlength}{1cm}",
                    "\\begin{picture}(4.5,5.5)(-0.25,-0.25)"]
                    ++ border
                    ++ concat (map (\(xy, square) -> draw xy square) (A.assocs p))
                    ++ ["\\end{picture}", "\\end{center}"]
    where   hline x0 x1 y = "\\put(" ++ (show $ min x0 x1) ++ "," ++ (show y)
                        ++ "){\\line(1,0){" ++ (show $ abs (x0 - x1)) ++ "}}"
            vline x y0 y1 = "\\put(" ++ (show x) ++ "," ++ (show $ min y0 y1)
                        ++ "){\\line(0,1){" ++ (show $ abs (y0 - y1)) ++ "}}"
            border = [hline (-0.1) 1 (-0.1), hline 3 4.1 (-0.1),
                    hline (-0.1) 4.1 5.1,
                    vline (-0.1) (-0.1) 5.1, vline 4.1 (-0.1) 5.1]
            rect (x0, y0) (x1, y1) = [hline x0 x1 y0, hline x0 x1 y1,
                                    vline x0 y0 y1, vline x1 y0 y1]
            shrinkRect (x0, y0) (x1, y1) = rect (x0' + s, y0' + s) (x1' - s, y1' - s)
                where   x0' = min x0 x1
                        x1' = max x0 x1
                        y0' = min y0 y1
                        y1' = max y0 y1
                        s = 0.1
            coordTrans (x, y) = (fromInteger $ toInteger x,
                        fromInteger $ toInteger $ 5 - y) :: (Double, Double)
            pRect xy0 xy1 = shrinkRect (coordTrans xy0) (coordTrans xy1)
            draw (x, y) Single = pRect (x, y) (x + 1, y + 1)
            draw (x, y) TwoTop = pRect (x, y) (x + 1, y + 2)
            draw (x, y) TwoLeft = pRect (x, y) (x + 2, y + 1)
            draw (x, y) FourTL = pRect (x, y) (x + 2, y + 2)
            draw _ _ = []
main = mapM_ (\(i, p) -> putStrLn (concat $ [c ++ "\n" | c <- (drawPos p i)]))
        $ zip [0..] $ winSequence startPos
      
{-
Main> putStrLn $ unlines (drawPos startPos 0)

\subsection*{After 0 steps}
\begin{center}
\setlength{\unitlength}{1cm}
\begin{picture}(4.5,5.5)(-0.25,-0.25)
\put(-0.1,-0.1){\line(1,0){1.1}}
\put(3.0,-0.1){\line(1,0){1.0999999999999996}}
\put(-0.1,5.1){\line(1,0){4.199999999999999}}
\put(-0.1,-0.1){\line(0,1){5.199999999999999}}
\put(4.1,-0.1){\line(0,1){5.199999999999999}}
\put(0.1,3.1){\line(1,0){0.8}}
\put(0.1,4.9){\line(1,0){0.8}}
\put(0.1,3.1){\line(0,1){1.8000000000000003}}
\put(0.9,3.1){\line(0,1){1.8000000000000003}}
\put(0.1,1.1){\line(1,0){0.8}}
\put(0.1,2.9){\line(1,0){0.8}}
\put(0.1,1.1){\line(0,1){1.7999999999999998}}
\put(0.9,1.1){\line(0,1){1.7999999999999998}}
\put(0.1,0.1){\line(1,0){0.8}}
\put(0.1,0.9){\line(1,0){0.8}}
\put(0.1,0.1){\line(0,1){0.8}}
\put(0.9,0.1){\line(0,1){0.8}}
\put(1.1,3.1){\line(1,0){1.7999999999999998}}
\put(1.1,4.9){\line(1,0){1.7999999999999998}}
\put(1.1,3.1){\line(0,1){1.8000000000000003}}
\put(2.9,3.1){\line(0,1){1.8000000000000003}}
\put(1.1,2.1){\line(1,0){1.7999999999999998}}
\put(1.1,2.9){\line(1,0){1.7999999999999998}}
\put(1.1,2.1){\line(0,1){0.7999999999999998}}
\put(2.9,2.1){\line(0,1){0.7999999999999998}}
\put(1.1,1.1){\line(1,0){0.7999999999999998}}
\put(1.1,1.9){\line(1,0){0.7999999999999998}}
\put(1.1,1.1){\line(0,1){0.7999999999999998}}
\put(1.9,1.1){\line(0,1){0.7999999999999998}}
\put(2.1,1.1){\line(1,0){0.7999999999999998}}
\put(2.1,1.9){\line(1,0){0.7999999999999998}}
\put(2.1,1.1){\line(0,1){0.7999999999999998}}
\put(2.9,1.1){\line(0,1){0.7999999999999998}}
\put(3.1,3.1){\line(1,0){0.7999999999999998}}
\put(3.1,4.9){\line(1,0){0.7999999999999998}}
\put(3.1,3.1){\line(0,1){1.8000000000000003}}
\put(3.9,3.1){\line(0,1){1.8000000000000003}}
\put(3.1,1.1){\line(1,0){0.7999999999999998}}
\put(3.1,2.9){\line(1,0){0.7999999999999998}}
\put(3.1,1.1){\line(0,1){1.7999999999999998}}
\put(3.9,1.1){\line(0,1){1.7999999999999998}}
\put(3.1,0.1){\line(1,0){0.7999999999999998}}
\put(3.1,0.9){\line(1,0){0.7999999999999998}}
\put(3.1,0.1){\line(0,1){0.8}}
\put(3.9,0.1){\line(0,1){0.8}}
\end{picture}
\end{center}

-}
