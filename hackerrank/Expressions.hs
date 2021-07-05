module Main where
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as M

-- creates Expression string  "3+4-2"
zipC l1 l2 = concat $ concat $ (map (uncurry mkL) $ zip l1 l2) ++ [[show $ last l1]]
zipC_ l = concat $ concat $ [[show $ fst $ head l]] ++ map (uncurry mkL_) (tail l)
mkL_ x y = [y] : show x : []
mkL x y = show x : [y] : []
-- *Main> concat $ (map (uncurry (\x y -> x : y : [])) $ zip ['1','2','3'] ['+','-']) ++ [['3']]
--"1+2-3"

-- creates Expression list  [(3,'+'),(4,'+'),(2,'-')]
zipL l1 l2 = zip l1 (['+'] ++ l2)
-- creates Expression list  [('+',3),('-',4),('+',2)]
zipR l1 l2 = zip l2 (init l1) ++ [('+', last l1)]

f x = case (snd x) of
    '*' -> ((*) $ fst x)
    '+' -> ((+) $ fst x)
    '-' -> ((\x y -> (-) y x) $ fst x)

-- all combinations of * + - with x digits - limited to first 2187
max_ = 9
max_combi = 3 ^ max_
combi x = go (3 ^ x - 1)
    where
    go 0 = [pattern 0]
    go n
        | n == (3 ^ x - max_combi) = []
        | otherwise = [pattern n] ++ go (n - 1)
    pattern n = go (x - 1) [] n
        where
        go 0 res nr = concat $ res ++ [cnvtdigt $ nr * 3 ^ 0]
        go pow res nr = go (pow - 1) (res ++ [cnvtdigt calcdigt]) (nr - calcdigt * 3 ^ pow)
            where
            calcdigt = nr `quot` (3 ^ pow)
        cnvtdigt x = case x of
            0 -> ['*']
            1 -> ['-']
            2 -> ['+']

unique :: (Ord a) => [a] -> [a]
unique xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

-- check all combinations of +-*  ... not computable (3^10000-1 combinations) ... only first 2187
makeExpressions t intL = go 0
    where
    exLL = combi (t - 1)
    go n
        | n == (length exLL - 1) = "ran out of '+-*' combinations (" 
            ++ show max_combi ++ " out of " ++ show (3 ^ (t - 1)) ++ ")"
        | calcChkL = zipC intL exL
        | otherwise = go (n + 1)
        where
        exL = exLL !! n
        calcChkL = foldl (\x y -> y x) 0 cnvtL `rem` 101 == 0
        cnvtL = map f $ zipL intL exL

-- calculate difference to 0 and try to find number in list
-- not working with lots of recurances of f.e. 1s
makeExpressions1 t intL = go 50 exL uIL
    where
    uIL = reverse $ sort $ unique intL
    exL = replicate t '+'
    go 0 mxL muIL = "no result - after 50 times"
    go n mxL [] = "no result - no more data"
    go n mxL muIL
        | calcChkL = zipC intL (tail mxL)
        | otherwise = go (n - 1) modi (tail muIL)
        where
        -- finding difference of result to 0 and find out which number to subtract to get result
        -- there must not be result, which is in list !!
        -- if not in list subtract first unique number of list, remove from unique list
        modi
            | (calc `quot` 2) `notElem` uIL && ((calc + 101) `quot` 2) `notElem` uIL = mmxL
            | even calc = findmod (calc `quot` 2)
            | otherwise = findmod ((calc + 101) `quot` 2)
        mmxL = replaceXMinus $ 1 + fromMaybe (-1) (find $ head muIL)
        findmod x = replaceXMinus $ 1 + fromMaybe (-1) (find x)
        replaceXMinus x
            | x == 0 = mxL
            | otherwise = take x mxL ++ ['-'] ++ drop (x + 1) mxL
        find x = elemIndex x (tail intL)
        calcChkL = calc == 0
        calc = foldl (\x y -> y x) 0 cnvtL `rem` 101
        cnvtL = map f $ zip intL mxL

-- calculate difference to 0 and try to find number in to map converted intL list - store map
-- if not found change first of uIL (nr) to '-', if still not found change next occurance of nr in intL
-- to '-', if no more occurance of nr in intL remove nr from uIL and proceed with next nr
-- - try this (50 `quot` nr + 1) times or until uIL is empty - restore map
-- if still not found, change first of muIL (nr) in intL to '*', then repeat above for each occurance of nr in 
-- intL. Reset last '*' to '+' (or '-') then set next to '*' avoiding too big numbers
-- if no more occurance delete nr (head) in muIL
-- in worst case until muIL is empty ... then go multiple set '*')
-- if nr is altered to '*' it should not be altered to '-' ! (check if '+' before change to '-')
makeExpressions2 t intL = gom []
    where
    posM = M.fromList $ zip [0..] intL
    valid mp k = mp M.! k == '+' && k /= 0
    -- create a map of all numbers in intL with a map of their positions and calculation signs
    lMap = lToMap intL exL   -- initial map
    exL = replicate t '+'
    cvL l = map f $ mapToL l
    -- "no result - after (50 `quot` nr + 1) times" - go to next number
    go 0 mMap mcnr muIL mnr_keys posL = go nnrx mMap mcnr upmuIL nmnr_keys posL
        where
        upmuIL = tail muIL
        nnr
            | tail muIL /= [] = head $ tail muIL
            | otherwise = (-1)
        nmnr_keys = filter (valid nnrMap) $ M.keys nnrMap
        nnrMap
            | nnr /= (-1) = mMap M.! nnr
            | otherwise = M.empty
        nnrx = 50 `quot` nnr + 1
    go n mMap mcnr [] mnr_keys posL
        | dcalcChkL = if dcalcChkL == calcChkL 
                    then (zipC_ $ mapToL mMap) else "empty IL - wrong result " ++ (zipC_ $ mapToL mMap)
        | otherwise = (gom posL)
        where
        dcalcChkL = dc1 == 0
        dc1 = mcnr `rem` 101
        calcChkL = c1 == 0
        c1 = cnr `rem` 101
        cnr = foldl (\x y -> y x) 0 cnvtL   -- calculated number from whole list
        cnvtL = map f $ mapToL mMap

    go n mMap mcnr muIL [] posL = go nnrx mMap mcnr upmuIL nmnr_keys posL
        where
        upmuIL = tail muIL
        nnr
            | tail muIL /= [] = head upmuIL
            | otherwise = (-1)
        nmnr_keys = filter (valid nnrMap) $ M.keys nnrMap
        nnrMap
            | nnr /= (-1) = mMap M.! nnr
            | otherwise = M.empty
        nnrx = 50 `quot` nnr + 1
    go n mMap mcnr muIL mnr_keys posL
        | dcalcChkL = if dcalcChkL == calcChkL 
                    then (zipC_ $ mapToL mMap) else "wrong result " ++ (zipC_ $ mapToL mMap)
        | mcnr < (-101) = gom posL
        | otherwise = go (n - 1) modi ccnr muIL upmnr_keys posL
        where
        -- finding difference of result to 0 and find out which number to subtract to get result
        -- there must not be result, which is in list !!
        mnr_key 
            | posL /= [] = last posL -- nr_key (position) of *
            | otherwise = 10000
        modi
            | cnrNotIn && cpnrNotIn = mx
            | cpnrNotIn = findmodX
            | otherwise = findmodXp
        ccnr
            | nr_key > mnr_key && (cnrNotIn && cpnrNotIn) = dcnr
            | nr_key <= mnr_key && (cnrNotIn && cpnrNotIn) = pcnr 
            | cpnrNotIn = if no_x_keys then pcnr else if head x_keys > mnr_key then xdcnr else pcnr
            | otherwise = if no_xp_keys then pcnr else if head xp_keys > mnr_key then xpdcnr else pcnr
        xdcnr = mcnr - 2 * x
        xpdcnr = mcnr + 2 * xp
        cnrNotIn = M.notMember x mMap
        cpnrNotIn = M.notMember xp mMap
        mx = M.insert nr updt_nrMap mMap
        updt_nrMap = M.insert nr_key '-' nrMap
        nr = head muIL
        nr_key = head mnr_keys
        nrMap = mMap M.! nr
        upmnr_keys = tail mnr_keys
        x_keys = filter (valid xMap) $ M.keys xMap
        xp_keys = filter (validp xpMap) $ M.keys xpMap
        validp mp k = mp M.! k == '-' && k /= 0
        xMap = mMap M.! x
        xpMap = mMap M.! xp
        no_x_keys = x_keys == []
        no_xp_keys = xp_keys == []
        findmodX
            | no_x_keys = mx
            | otherwise = M.insert x updt_xMap mMap
            where
            updt_xMap = M.insert nx_key '-' xMap
            nx_key = head x_keys
        findmodXp
            | no_xp_keys = mx
            | otherwise = M.insert xp updt_xpMap mMap
            where
            updt_xpMap = M.insert nx_key '+' xpMap
            nx_key = head xp_keys
        x
            | even dc1 = dc1 `quot` 2
            | otherwise = (dc1 + 101)  `quot` 2
        xp = 101 - x
        dcalcChkL = dc1 == 0
        dc1 = mcnr `rem` 101
        dcnr = mcnr - 2 * nr
        calcChkL = c1 == 0
        c1 = cnr `rem` 101
        cnr = foldl (\x y -> y x) 0 cnvtL   -- calculated number from whole list
        cnvtL = map f $ mapToL mMap
        pcnr = foldl (\x y -> y x) 0 pcnvtL   -- precalculated number from whole list
        pcnvtL = map f $ mapToL modi

    -- posL ... position list of all *
    -- posM ... position map holding int numbers - position is key
    gom posL
        | length posL < t = (go bmnrx mmMap bmcnr bmmuIL bmmnr_keys nPosL)
        | otherwise = "no result"
        where
        bmnrx = 50 `quot` bmnr + 1
        bmmuIL = M.keys lMap
        bmnr = head bmmuIL
        bmmnr_keys = filter (valid bmnrMap) $ M.keys bmnrMap
        bmnrMap = lMap M.! bmnr
        bmcnr = foldl (\x y -> y x) 0 (cvL mmMap)   -- basic calculated number from whole list

        mmMap = gox posL lMap m where
            gox [] mM l = mM
            gox pL mM l = gox (init pL) nmM (l - 1)
                where
                nmM = M.insert nr updt_nrMap mM
                nr = posM M.! pos
                pos = pL !! (l - 1)
                updt_nrMap = M.insert pos '*' nrMap
                nrMap = lMap M.! nr
        m = length posL
        nPosL
            | posL /= [] = nextPosL
            | otherwise = [1]
        nextPosL = gop posL [] m where
            gop [] _ l = [x | x <- [1..m]] ++ [m + 1]
            gop mPosL lPosL l
                | mPosL !! (l - 1) < t - m + l - 1 = init mPosL ++ [mPosL !! (l - 1) + 1] ++ lPosL
                | otherwise = gop (init mPosL) ([last mPosL] ++ lPosL) (l - 1)

--7
--33 55 45 67 74 54 48
--33*55*45+67+74-54+48
--makeExpressions2 10 [46,47,48,49,50,51,51,52,53,54]
--"46*47*48-49-50-51+51-52-53+54"

lToMap iL eL = go 0 iL eL lM
    where
    lM = M.empty
    go cnt miL meL lM  -- [(3, M.fromList[(5,'+')])]
        | cnt == length iL = M.map M.fromList lM
        | M.lookup nr lM == Nothing = go (cnt + 1) (tail miL) (tail meL) (M.insert nr [(cnt, sn)] lM)
        | otherwise = go (cnt + 1) (tail miL) (tail meL) (M.adjust (++ [(cnt,sn)]) nr lM)
        where
        nr = head miL
        sn = head meL

mapToL m = map g $ sort $ concat $ map f $ M.toList $ M.map M.toList m
--mapToL m = M.toList $ M.map M.toList m
    where
    f (nr, pcL) = map (\(p, c) -> (p, nr, c)) pcL
    g (pos, nr, char) = (nr, char)

makeExpressions_ t intL
    | t < (max_ + 2) = makeExpressions t intL
    | otherwise = makeExpressions2 t intL

main :: IO()
main = do
    t <- readLn :: IO Int
    intL <- fmap (map (read :: String -> Int).words) getLine
    putStrLn $ makeExpressions2 t intL
