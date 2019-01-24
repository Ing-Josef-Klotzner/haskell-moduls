module Main where
import Control.Monad (liftM, replicateM)
import qualified Data.Vector as V
import qualified Data.Map as M
type SMap = M.Map Int Int 
type Group = V.Vector (V.Vector Int)

readLst :: IO [Int]
readLst = liftM (map read . words) getLine

main :: IO ()
main = do
    -- m ... count of prisoners
    m <- readLn :: IO (Int)
    -- n ... count of prisoner pairs
    n <- readLn :: IO (Int)
    listL <- replicateM n readLst
    -- check if prisoner is in any group - via search map
    --    if in group, add the other prisoner of pair to this group
    --      and add to search map to find with prisoner to which group it belongs
    --    if no prisoner of pair is in group, create new group and add both of pair to group
    --      and add both to search map to find with prisoner to which group it belongs
    --  remaining prisoners (in no group) are cost 1 for each
    let costs = go listL 0 where
            go [] cost = cost
            go pList cost
                | True = undefined
                where
                [p1,p2] = head pList
    print listL
    
