module Main where
import Debug.Trace
import Control.Monad
import Control.Monad.State (State, get, gets, put, modify, evalState)
import Data.List (intersperse)
import Data.Map (Map, fromList, unions, lookup, insert)
import qualified Data.Map as M
-- there are 137 loosing and 3139 winning states in chomp 3/25
-- Complete the solve function below.
loseL = [(1, 0, 0), (2, 1, 0), (2, 2, 1), (3, 1, 1), (3, 2, 0), (4, 2, 2), (4, 3, 0), (5, 3, 2),
        (5, 4, 0), (5, 5, 3), (6, 3, 3), (6, 4, 2), (6, 5, 0), (7, 4, 3), (7, 5, 2), (7, 6, 0),
        (7, 7, 4), (8, 4, 4), (8, 6, 2), (8, 7, 0), (9, 5, 4), (9, 6, 5), (9, 7, 2), (9, 8, 0),
        (9, 9, 6), (10, 5, 5), (10, 6, 4), (10, 8, 2), (10, 9, 0), (11, 6, 6), (11, 7, 5),
        (11, 9, 2), (11, 10, 0), (12, 7, 6), (12, 8, 5), (12, 9, 7), (12, 10, 2), (12, 11, 0),
        (12, 12, 8), (13, 7, 7), (13, 8, 6), (13, 9, 5), (13, 11, 2), (13, 12, 0), (14, 8, 7),
        (14, 9, 8), (14, 10, 5), (14, 11, 9), (14, 12, 2), (14, 13, 0), (14, 14, 10), (15, 8, 8),
        (15, 10, 7), (15, 11, 5), (15, 13, 2), (15, 14, 0), (16, 9, 9), (16, 10, 8), (16, 11, 7),
        (16, 12, 5), (16, 14, 2), (16, 15, 0), (17, 10, 9), (17, 11, 8), (17, 12, 7), (17, 13, 5),
        (17, 14, 11), (17, 15, 2), (17, 16, 0), (17, 17, 12), (18, 10, 10), (18, 12, 9),
        (18, 13, 7), (18, 14, 5), (18, 16, 2), (18, 17, 0), (19, 11, 10), (19, 12, 11),
        (19, 13, 9), (19, 14, 7), (19, 15, 5), (19, 16, 12), (19, 17, 2), (19, 18, 0),
        (19, 19, 13), (20, 11, 11), (20, 12, 10), (20, 14, 9), (20, 15, 7), (20, 16, 5),
        (20, 18, 2), (20, 19, 0), (21, 12, 12), (21, 13, 10), (21, 15, 9), (21, 16, 7),
        (21, 17, 5), (21, 18, 11), (21, 19, 2), (21, 20, 0), (22, 13, 11), (22, 14, 12),
        (22, 15, 13), (22, 16, 9), (22, 17, 7), (22, 18, 5), (22, 19, 14), (22, 20, 2),
        (22, 21, 0), (22, 22, 15), (23, 13, 12), (23, 14, 13), (23, 15, 11), (23, 16, 14),
        (23, 17, 9), (23, 18, 7), (23, 19, 5), (23, 20, 15), (23, 21, 2), (23, 22, 0),
        (23, 23, 16), (24, 13, 13), (24, 15, 12), (24, 16, 11), (24, 18, 9), (24, 19, 7),
        (24, 20, 5), (24, 22, 2), (24, 23, 0), (25, 14, 14), (25, 16, 13), (25, 17, 11),
        (25, 19, 9), (25, 20, 7), (25, 21, 5), (25, 23, 2), (25, 24, 0)]
-- a loose state mini list - easy to investigate by manual try
loseS = [(1, 0, 0), (2, 1, 0), (2, 2, 1), (3, 1, 1)]
-- True means "WIN", False means "LOSE"
loseM :: Map (Int, Int, Int) Bool
loseM = fromList [((1, 0, 0),False), ((2, 1, 0),False), ((2, 2, 1),False), ((3, 1, 1),False)]
-- Map.lookup (1,0,0) loseM    Output: Just False

moves :: (Enum t, Num t, Ord t) => (t, t, t) -> [(t, t, t)]
moves (a,b,c) = [(x,y,z) 
                | x <- [0 .. a], 
                y <- if x == a then [0 .. b] else if x < b then [x] else [b], 
                z <- if x == a && y == b then [0 .. c - 1] else if y < c then [y] else [c]] 

-- using a already calculated loose list is fastest way
solve :: (Int, Int, Int) -> [Char]
solve (a,b,c)
    | (a,b,c) `elem` loseL = "LOSE"
    | otherwise = "WIN"

-- slowest version - wins_ (7,7,7) takes 6 sec.
wins_ :: (Int, Int, Int) -> Bool
wins_ (0,0,0) = True
wins_ stat = any (not . wins_) (moves stat)

-- little faster by using a short lookup map for known lose states - winsM (7,7,7) takes 1 sec.
winsM :: (Int, Int, Int) -> Bool
winsM (0,0,0) = True
winsM stat = 
    case M.lookup stat loseM of
        Just w -> w
        Nothing -> do
            let res = any (not . winsM) (moves stat)
--            let loseM = M.insert stat res loseM   -- useless
            res

-- not used function for memoization a result into a map mp
memoize :: Ord a => Map a b -> (Map a b -> a -> (Map a b, b)) -> a -> (Map a b, b)
memoize mp f x = case M.lookup x mp of
    (Just y) -> (mp, y)
    Nothing  -> (mp'', y) where
        (mp', y) = f mp x
        mp''     = M.insert x y mp'

-- need to change to monad function for using memoizeM
winsX_ :: Monad m => ((Int, Int, Int) -> m Bool) -> (Int, Int, Int) -> m Bool
winsX_ f (0,0,0) = pure True
winsX_ f stat = do
    b <- not . and <$> traverse f (moves stat)
    pure b
wins :: (Int, Int, Int) -> Bool
wins state = memoizeM winsX_ state

--Prelude> not . any id $ (==1) <$> [1,2,3]  -- <$> is fmap
--False
--Prelude> not . any id $ (==1) <$> [0,2,3]
--True

-- working memoization on Monad example of fibonacci
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

fibM :: Monad m =>
        (Integer -> m Integer)
      -> Integer -> m Integer
fibM f 0 = return 1
fibM f 1 = return 1
fibM f n = do
  a <- f (n-1)
  b <- f (n-2)
  return (a+b)

fib n = memoizeM fibM n


-- working memoization example of fibonacci
stateMemoFibs :: Int -> State (Map Int Integer) Integer
stateMemoFibs 0 = return 0
stateMemoFibs 1 = return 1
stateMemoFibs n = do
  -- Try and get the n-2 and n-1 fib from the cache. If they're not there, 
  -- calculate them recursively and update the cache.
  n2 <- getOrUpdate (n-2) (stateMemoFibs (n-2))
  n1 <- getOrUpdate (n-1) (stateMemoFibs (n-1))
  return (n2 + n1)

getOrUpdate :: (Ord k) => k -> State (Map k v) v -> State (Map k v) v
getOrUpdate k ifEmptyState = do
    maybeVal <- gets (M.lookup k)
    case maybeVal of
        Just v -> return v
        Nothing -> do
            ifEmpty <- ifEmptyState
            modify (M.insert k ifEmpty)
            return ifEmpty

fibGoU :: Int -> Integer
fibGoU x = evalState (stateMemoFibs x) M.empty



solve' :: (Int, Int, Int) -> [Char]
solve' stat
    | wins stat = "WIN"
    | otherwise = "LOSE"

-- function to show board
sho :: (Int, Int, Int) -> IO ()
sho (a,b,c) = do
    if b > a || c > a || c > b then putStrLn "Board fault! c <= b <= a in (a,b,c)!" else putStrLn ""
    let line xc = snd xc ++ "|" ++ intersperse ' ' (replicate (fst xc) 'x')
    mapM_ putStrLn $ map line [(c,"3"),(b,"2"),(a,"1")]

main :: IO()
main = do
    n <- readLn :: IO Int
    forM_ [1..n] (\_ -> do
        tc <- fmap ((\[a, b, c] -> (a, b, c)).map (read::String->Int).words) getLine 
        putStrLn $ solve' tc)


--Sample Input

--2
--1 1 1
--2 2 1
--Sample Output

--WIN
--LOSE

--in
--5
--14 12 2
--24 13 13
--3 1 1
--21 15 11
--4 3 0

--out
--LOSE
--LOSE
--LOSE
--WIN
--LOSE

--my out
--WIN
--WIN
--LOSE
--WIN
--LOSE

