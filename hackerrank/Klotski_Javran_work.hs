{-# LANGUAGE TupleSections #-}
import Control.Applicative
import Control.Monad hiding (mapM_)
import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.List (delete)
import Data.Ix
import Data.Bits
import Data.Int
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Prelude hiding
    ( minimum
    , concat
    , notElem
    , concatMap
    , all
    , mapM_
    , foldr
    )

type Coord = (Int, Int)
type Block = (Coord, Shape)
type Box = M.Map String Block
type Move = (String, (Coord, Coord))

-- | a shape is now tagged with a "hashcode"
--   knowing a shape will at most be of 3x3 size,
--   16 bits are just sufficient to store it
data Shape = Shape (S.Set Coord) !Int16
    deriving Show

-- use the "hashcode" to avoid comparision
-- on the Set structure
instance Eq Shape where
    Shape _ h1 == Shape _ h2 = h1 == h2

instance Ord Shape where
    Shape _ h1 `compare` Shape _ h2 = h1 `compare` h2

data Puzzle = Puzzle
    { pzM           :: Int    -- ^ puzzle size MxN
    , pzN           :: Int
    , pzBox         :: Box    -- ^ initial box
    , targetBlkName :: String
    , targetCoord   :: Coord
    } deriving Show

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons ~(a:as) = Just (a,as)

readPairLn :: (Read a, Read b) => IO (a,b)
readPairLn = do
    (a:b:_) <- words <$> getLine
    return (read a, read b)

readPuzzle :: IO Puzzle
readPuzzle = do
    (m,n) <- readPairLn
    b <- readBox m n
    Puzzle m n b <$> getLine    -- target block
                 <*> readPairLn -- target coord

-- | encode shapes into numbers
shapeHash :: Foldable f => f Coord -> Int16
shapeHash = foldr (\(x,y) -> (`setBit` (3*x+y))) 0

-- | from old shape to new shape
buildShape :: S.Set Coord -> Shape
buildShape = Shape <*> shapeHash

-- | convert from a list of coordinates
--   to a block
coordsToBlock :: [Coord] -> Block
coordsToBlock xs = (topLeft, buildShape spCoords')
    where
      spCoords' = S.fromList . map (subtract x *** subtract y) $ xs
      topLeft@(x,y) = (minimum . map fst &&& minimum . map snd) xs

-- | convert from a block
--   to a list of coordinates
blockToCoords :: Block -> [Coord]
blockToCoords ((x,y), Shape cs _) = map ((+ x) *** (+ y)) $ toList cs

-- | read and parse initial box from input
readBox :: Int -> Int -> IO Box
readBox m n = toBox . concat <$> forM [0..m-1] readRow
    where
      readRow :: Int -> IO [(String, Coord)]
      -- get every single cell name paired with its coordinate
      readRow row = zipWith (\col w -> (w,(row,col))) [0..n-1]
                  . words <$> getLine
      toBox :: [(String, Coord)] -> Box
      toBox = fmap coordsToBlock
            . M.fromListWith (++)         -- cluster coordinates by names
            . map (second (:[]))
            . filter (notElem  '.' . fst) -- dots are empty cells

-- | unsafe map lookup
find' :: Ord a => a -> M.Map a b -> b
find' x = fromJust . M.lookup x

-- | perform BFS to solve the puzzle
search :: Puzzle
       -> Seq.Seq (Box,[Move])      -- ^ BFS queue
       -> S.Set (Coord,S.Set Block) -- ^ visited states (digested)
       -> Int
       -> (Box,[Move])              -- ^ return one solution
search puz todoList visited n = case Seq.viewl todoList of
    Seq.EmptyL -> error "solution not found"
    (b,mvs) Seq.:< bs ->
        let  newVisited = S.insert (digestBox b) visited
             targetBlock = find' (targetBlkName puz) b
             -- the state is digested by wiping out all block names
             -- but keep the coordinate of the target block
             -- in case the target block happens to have the same shape
             -- of a non-target block
             digestBox :: Box -> (Coord, S.Set Block)
             digestBox b' = ( fst targetBlock
                            , S.fromList . M.elems $ b')
        in case () of
            -- skipping visited states
        _ | digestBox b `S.member` visited -> search puz bs visited n
            -- a solution is found
        _ | fst targetBlock == targetCoord puz -> (b,mvs)-- error $ "solution found in round: " ++ show n ++ "\nitems todoList: " ++ show (length $ toList todoList) ++ "\n" --(b,mvs)
        _ | n == 99999 -> error $ "round " ++ show n ++ "\n" ++ show mvs ++ "\n" ++ show (map (\(_,m) -> m) (toList todoList))
            -- expand the current queue, and search next one in the queue
        _ -> search puz (bs Seq.>< nextMoves (pzM puz) (pzN puz) (b,mvs)) newVisited (n+1)

-- | given one state with its move history
--   get all possibilities of its next state
nextMoves :: Int -> Int -> (Box,[Move]) -> Seq.Seq (Box,[Move])
nextMoves m n (b,mvs) = do
    -- select one block
    blkName  <- Seq.fromList $ {-case uncons mvs of
        Nothing -> -} M.keys b
        -- but if we have a previous move, exclude that move
        -- so we don't end up with cycles
--        Just ((mv,_),_) -> delete mv (M.keys b)
    let curBlock = find' blkName b
        -- remove the selected block from box
        remainingBox = M.delete blkName b
        -- we need to tell whether a given coordinate has been occupied
        -- given the whole box is at most of size 6x6 = 36,
        -- 64 bits are sufficient to keep all the information we need
        occupiedCoords :: Int64
        occupiedCoords = foldr (\(x,y) -> (`setBit` (6*x+y))) 0 -- encoded
                       . concatMap blockToCoords -- remaining coordinates
                       . M.elems $ remainingBox
        notOccupied :: Coord -> Bool
        notOccupied (x,y) = not $ testBit occupiedCoords (6*x+y)
        -- do another BFS to get a list of possibilities reached
        -- by just moving this selected block (expand)
        expand :: Seq.Seq Block -> S.Set Block -> S.Set Block
        expand blkSeq visited = case Seq.viewl blkSeq of
            Seq.EmptyL -> visited
            nxtBlk Seq.:< nxtBlks ->
                let nxtCoords = blockToCoords nxtBlk
                    validCoord = (&&) <$> inRange ((0,0),(m-1,n-1))
                                      <*> notOccupied
                in case () of
                        -- if the current block position has been visited
                    _ | nxtBlk `S.member` visited -> expand nxtBlks visited
                        -- if all coordinates are valid (see above)
                        -- then mark it as visited and expand the queue
                    _ | all validCoord nxtCoords
                        -> expand (nxtBlks Seq.>< fmap (,snd nxtBlk) (nextCoords . fst $ nxtBlk))  --Seq.filter (\ coord -> (fst nxtBlk) /= coord) $ 
                            (S.insert nxtBlk visited)
                        -- not a valid block position, try next one
                    _ -> expand nxtBlks visited
    -- get all possible moves using this selected block
    fmap (\nxtBlk -> ( M.insert blkName nxtBlk remainingBox -- plug in this block
                    ,(blkName ,( fst curBlock               -- update move history
                               , fst nxtBlk) ):mvs))
        . Seq.fromList
--        . toList                                          -- orig
--        $ expand (return curBlock) S.empty                -- orig
        $ filter (\(coord,shape) -> coord /= fst curBlock)  -- corr
        $ toList $ expand (return curBlock) S.empty         -- corr
{-
originally:
solution found in round: 16215
items todoList: 21250
real	0m57.564s
user	0m48.952s
sys	0m8.816s

corrected:
solution found in round: 16215
items todoList: 4620
real	0m46.809s
user	0m46.820s
sys	0m0.120s

error was unnecessary "nomoves" (from / to same position) in todoList:
[[("BB",((4,3),(4,1)))],[("BB",((4,3),(4,2)))],[("BB",((4,3),(4,3)))],
                                                       -----------    
[("BS",((0,1),(0,1)))],[("BW",((3,0),(3,0)))],[("DS",((3,2),(3,2)))],
        -----------             ---------              ---------
[("DS",((3,2),(4,1)))],[("DS",((3,2),(4,2)))],[("DW",((3,1),(3,1)))],
                                                      -----------
[("DW",((3,1),(4,1)))],[("DW",((3,1),(4,2)))],[("KS",((4,0),(4,0)))],
                                                      -----------
[("KS",((4,0),(4,1)))],[("KS",((4,0),(4,2)))],[("KW",((3,3),(3,3)))],
                                                      ------------  
[("LS",((2,3),(2,3)))],[("LW",((2,2),(2,2)))],[("RS",((0,3),(0,3)))],
        -----------            -----------            -----------
[("RW",((0,0),(0,0)))],[("TS",((2,1),(2,1)))],[("TW",((2,0),(2,0)))]]
        -----------            ----------             ----------
-}

-- | all possible next coordinates
nextCoords :: Coord -> Seq.Seq Coord
nextCoords (x,y) = Seq.fromList $ nextCoords'' (x,y)
nextCoords'' (x,y) = fmap ((+ x) *** (+ y))
                 $ [(-1,0), (1,0), (0,-1), (0,1)]

-- | pretty print a move history
printMove :: Move -> IO ()
printMove (s,(c1,c2)) = putStrLn $ unwords [s, show c1, show c2]

main :: IO ()
main = do
    pz <- readPuzzle
    let moves = reverse
              . snd
              $ search pz (return (pzBox pz,[])) S.empty 0
--    nextMoves (pzM pz) (pzN pz) (return (pzBox pz,[]))
    print (length moves)
    mapM_ printMove moves

{-
3 4
. A . .
A B . C
. . . C
B
0 0
*** Exception: solution found in round: 46
items todoList: 297
-}
