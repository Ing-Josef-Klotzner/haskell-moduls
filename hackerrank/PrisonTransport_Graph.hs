module Main where
import Data.Tree (flatten)
import qualified Data.Graph as G
import Control.Monad (liftM, replicateM)
import qualified Data.Vector as V
import qualified Data.Map as M
type SMap = M.Map Int Int 
type Group = V.Vector (V.Vector Int)

readLst :: IO [Int]
readLst = liftM (map read . words) getLine

-- x ... prisoner count   gCost ... costs per bus
gCost :: Int -> Int
gCost x = (fromEnum sqrtX) + roundUp
    where
    sqrtX = sqrt (toEnum x)
    isInt = sqrtX - (toEnum $ fromEnum sqrtX) == 0
    roundUp
        | isInt = 0
        | True = 1

groupCosts :: G.Graph -> [Int]
groupCosts g = map (gCost . length . flatten) $ G.components g

-- map T.flatten (Data.Graph.components (Data.Graph.buildG (1,6) [(1,2),(3,4),(5,6),(3,5)]))
--[[1,2],[3,4,5,6]]
main :: IO ()
main = do
    -- m ... count of prisoners
    m <- readLn :: IO (Int)
    -- n ... count of prisoner pairs
    n <- readLn :: IO (Int)
    listL <- replicateM n readLst
    let listTl = map toTup listL
        toTup [x, y] = (x, y)
        prisoners = G.buildG (1, m) listTl
        costs_ = sum $ groupCosts prisoners

    -- check if prisoner is in any group - via search map
    --    if none in a group create new group and add them, add them also to search map
    --    if both in same group, do nothing
    --    if both in different group, merge group2 to 1, make group 2 emtpy, 
    --         change all of 2 to point to group 1   
    --    if in group, add the other prisoner of pair to this group
    --      and add to search map to find with prisoner to which group it belongs
    --    if no prisoner of pair is in group, create new group and add both of pair to group
    --      and add both to search map to find with prisoner to which group it belongs
    --  remaining prisoners (in no group) are cost 1 for each
        costs = go listL (M.empty :: SMap) (V.empty :: Group) 0 where
            go :: [[Int]] -> SMap -> Group -> Int -> Int
            go [] smap grp grpNr = cost
                where
                cost = V.sum grpCost + (m - M.size smap)
                grpCost :: V.Vector Int
                grpCost = V.map (gCost . V.length) grp
                gCost :: Int -> Int
                gCost x = (fromEnum sqrtX) + roundUp
                    where
                    sqrtX = sqrt (toEnum x)
                    isInt = sqrtX - (toEnum $ fromEnum sqrtX) == 0
                    roundUp
                        | isInt = 0
                        | True = 1
            go pList smap grp grpNr
                | not p1In && not p2In = go (tail pList) insP1_p2 p1_p2ToGrp (grpNr + 1)
                -- merge p1Grp and p2Grp to p1Grp
                | p1In && p2In && p1Grp == p2Grp = go (tail pList) smap grp grpNr
                | p1In && p2In = go (tail pList) mergedSmap mergedGrpD grpNr
                | p1In = go (tail pList) (M.insert p2 p1Grp smap) (xToGrp p2 p1Grp) grpNr
                | p2In = go (tail pList) (M.insert p1 p2Grp smap) (xToGrp p1 p2Grp) grpNr
                where
                mergeP2grp = grp V.! p2Grp
                mergedSmap = V.foldr (\x y -> M.insert x p1Grp y) smap mergeP2grp
                mergedGrp = grp V.// [(p1Grp, (grp V.! p1Grp) V.++ (grp V.! p2Grp))]
                mergedGrpD = mergedGrp V.// [(p2Grp, V.empty)]
                [p1,p2] = head pList
                insP1_p2 = M.insert p2 grpNr (M.insert p1 grpNr smap)
                p1_p2ToGrp = V.snoc grp (V.fromList [p1, p2])
                xToGrp x gp = grp V.// [(gp, V.snoc (grp V.! gp) x)]
                mBp1 = M.lookup p1 smap
                mBp2 = M.lookup p2 smap
                p1In = case mBp1 of
                    Nothing -> False
                    Just _ -> True
                p2In = case mBp2 of
                    Nothing -> False
                    Just _ -> True
                p1Grp = case mBp1 of
                    Nothing -> 0
                    Just gp1 -> gp1
                p2Grp = case mBp2 of
                    Nothing -> 0
                    Just gp2 -> gp2
-- version with costs too slow (testcase 9 processes 10 min! and uses 5 GB !)
--    print costs
    print costs_

{-
16
8
6 11
9 5
11 9
15 9
13 15
12 14
15 16
1 16

answer: 11

80
30
40 22
60 6
22 39
43 40
22 55
48 57
42 41
22 57
6 42
33 74
70 46
4 11
6 28
22 79
61 34
77 40
4 8
72 26
62 50
72 51
1 79
34 29
77 41
2 48
43 2
62 45
43 17
19 33
76 4
35 54

answer: 62
-}
