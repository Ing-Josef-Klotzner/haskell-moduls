module Main where
import Data.Tree (flatten)
import qualified Data.Graph as G
import Control.Monad (liftM, forM_, replicateM)
import qualified Data.Vector.Mutable as MV
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
    let listV = V.fromList listL
        grpV = V.fromList $ [[x] | x <- [0 .. m]] :: V.Vector [Int]
    -- search vector for finding group
    sMV <- V.unsafeThaw $ V.fromList [0 .. m]
    grpMV <- V.unsafeThaw grpV
    MV.write grpMV 0 []  -- ... 0 is no member 
    forM_ [0 .. n - 1] $ (\x -> do
        let [p1, p2] = listV V.! x
--        print (p1, p2)
        -- points to group - points to list with itself (length == 1) or to linked group
        p1G <- MV.read sMV p1
        p2G <- MV.read sMV p2
        -- group lists
        p1GL <- MV.read grpMV p1G
        p2GL <- MV.read grpMV p2G
        
        case () of
            _
                -- both not linked -> link p2 in same group as p1
                | [p1G] == p1GL && [p2G] == p2GL -> do
                    MV.write sMV p2 p1G
                    MV.write grpMV p1G [p1, p2]
                    MV.write grpMV p2G []
                -- both linked and in same group -> nothing to do
                | [p1G] /= p1GL && [p2G] /= p2GL && p1G == p2G -> return ()
                -- p1 linked and p2 linked, different groups -> merge groups to p1G
                | [p1G] /= p1GL && [p2G] /= p2GL -> do
                    forM_ p2GL $ (\x -> MV.write sMV x p1G)
                    MV.write grpMV p1G (p1GL ++ p2GL)
                    MV.write grpMV p2G []
--                    putStrLn $ "merged p2 to p1G " ++ show (p1GL ++ p2GL) ++ " p1 " ++ show p1
--                        ++ " p2 " ++ show p2 ++ " p1G " ++ show p1G ++ " p2G " ++ show p2G
--                        ++ " p1GL " ++ show p1GL ++ " p2GL " ++ show p2GL
                -- just p1 is linked -> link p2 to p1G
                | [p1G] /= p1GL -> do
                    MV.write sMV p2 p1G
                    MV.write grpMV p1G (p1GL ++ [p2])
                    MV.write grpMV p2G []
                -- just p2 is linked -> link p1 to p2G
                | [p2G] /= p2GL -> do
                    MV.write sMV p1 p2G
                    MV.write grpMV p2G (p2GL ++ [p1])
                    MV.write grpMV p1G []                    
        )
    grpVRes <- V.freeze grpMV
    let costs' = V.sum $ V.map (gCost' . length) grpVRes  
        gCost' :: Int -> Int
        gCost' x = (fromEnum sqrtX) + roundUp
            where
            sqrtX = sqrt (toEnum x)
            isInt = sqrtX - (toEnum $ fromEnum sqrtX) == 0
            roundUp
                | isInt = 0
                | True = 1
        
        listTl = map toTup listL
        toTup [x, y] = (x, y)
        prisoners = G.buildG (1, m) listTl
        costs_ = sum $ groupCosts prisoners

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
--    print costs_
    print costs'

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
