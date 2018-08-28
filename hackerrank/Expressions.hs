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

-- only for testing
intL = [55,3,45,33,25]
exL = ['+','+','-','-']
cvL = cvtL intL exL
cvtL iL eL = map f $ zipL iL eL
f x = case (snd x) of
    '*' -> ((*) $ fst x)
    '+' -> ((+) $ fst x)
    '-' -> ((\x y -> (-) y x) $ fst x)
--eL = filter fi rL
fi x = x == '+' || x == '-' || x == '*'

-- *Main> (foldl (\x y -> y x) 0 $ cvtL iL eL) `rem` 101
--0
-- *Main Control.Applicative> [(1 +)] <*> [3,4]
--[4,5]
-- *Main Control.Applicative> foldl (\x y -> y x) 0 $ cvtL [1,2,3] ['+','-']
--0
-- all combinations of * + - with x digits - limited to first 2187
max_combi = 3 ^ 7
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
makeExpressions2 t intL = (go bnrx lMap lMap bcnr bmuIL bmnr_keys ([bnr] ++ bmuIL))
    where
    bnrx = 50 `quot` bnr + 1
    bnr = head bmuIL
    bmuIL = M.keys lMap
    bmnr_keys = filter (valid bnrMap) $ M.keys bnrMap
    bnrMap = lMap M.! bnr
    valid mp k = mp M.! k == '+' && k /= 0
    -- create a map of all numbers in intL with a map of their positions and calculation signs
    lMap = lToMap intL exL   -- initial map
    exL = replicate t '+'
    bcnr = foldl (\x y -> y x) 0 (cvL lMap)   -- basic calculated number from whole list
    cvL l = map f $ mapToL l
    -- "no result - after (50 `quot` nr + 1) times"
    go 0 sMap mMap mcnr muIL mnr_keys mmuIL = go nnrx sMap mMap mcnr upmuIL nmnr_keys mmuIL
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
    go n sMap mMap mcnr [] mnr_keys mmuIL
        | dcalcChkL = if dcalcChkL == calcChkL 
                    then (zipC_ $ mapToL mMap) else "empty IL - wrong result " ++ (zipC_ $ mapToL mMap)
        | otherwise = gom sMap mmuIL
        where
        dcalcChkL = dc1 == 0
        dc1 = mcnr `rem` 101
        calcChkL = c1 == 0
        c1 = cnr `rem` 101
        cnr = foldl (\x y -> y x) 0 cnvtL   -- calculated number from whole list
        cnvtL = map f $ mapToL mMap

    go n sMap mMap mcnr muIL [] mmuIL = go nnrx sMap mMap mcnr upmuIL nmnr_keys mmuIL
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
    go n sMap mMap mcnr muIL mnr_keys mmuIL
        | dcalcChkL = if dcalcChkL == calcChkL 
                    then (zipC_ $ mapToL mMap) else "wrong result " ++ (zipC_ $ mapToL mMap)
        | mcnr < 0 = gom sMap mmuIL
        | otherwise = go (n - 1) sMap modi ccnr muIL upmnr_keys mmuIL
        where
        -- finding difference of result to 0 and find out which number to subtract to get result
        -- there must not be result, which is in list !!
        mnr_key 
            | mmuIL /= [] = head $ M.keys (mMap M.! (head mmuIL)) -- nr_key (position) of *
            | otherwise = 10000
        modi
            | cnrNotIn = mx
            | otherwise = findmod x
        ccnr  --   = pcnr
            | nr_key > mnr_key && cnrNotIn = dcnr
            | nr_key <= mnr_key && cnrNotIn = pcnr 
            | otherwise = if no_x_keys then pcnr else if head x_keys > mnr_key then xdcnr else pcnr
--            | cnrNotIn = dcnr
--            | otherwise = if no_x_keys then dcnr else xdcnr
        xdcnr = mcnr - 2 * x
        cnrNotIn = M.notMember x mMap
        mx = M.insert nr updt_nrMap mMap
        updt_nrMap = M.insert nr_key '-' nrMap
        nr = head muIL
        nr_key = head mnr_keys
        nrMap = mMap M.! nr
        upmnr_keys = tail mnr_keys
        x_keys = filter (valid xMap) $ M.keys xMap
        xMap = mMap M.! x
        no_x_keys = x_keys == []
        findmod x
            | no_x_keys = mx
            | otherwise = M.insert x updt_xMap mMap
            where
            updt_xMap = M.insert nx_key '-' xMap
            nx_key = head x_keys
        x
            | even dc1 = dc1 `quot` 2
            | otherwise = (dc1 + 101)  `quot` 2
        dcalcChkL = dc1 == 0
        dc1 = mcnr `rem` 101
        dcnr = mcnr - 2 * nr
        calcChkL = c1 == 0
        c1 = cnr `rem` 101
        cnr = foldl (\x y -> y x) 0 cnvtL   -- calculated number from whole list
        cnvtL = map f $ mapToL mMap
        pcnr = foldl (\x y -> y x) 0 pcnvtL   -- precalculated number from whole list
        pcnvtL = map f $ mapToL modi
    gom pmMap [] = "to be written multiple '*'"
    gom pmMap mmuIL = (go bmnrx mmMap mmMap bmcnr bmmuIL bmmnr_keys mmmuIL)
        where
        bmnrx = 50 `quot` bmnr + 1
        bmmuIL = M.keys mmMap
        bmnr = head bmmuIL
        bmmnr_keys = filter (valid bmnrMap) $ M.keys bmnrMap
        bmnrMap = mmMap M.! bmnr
        bmcnr = foldl (\x y -> y x) 0 (cvL mmMap)   -- basic calculated number from whole list

        nr = head mmuIL
        rmMap = M.insert nr updt_nrMap pmMap
        updt_nrMap
            | no_nr_key = nrMap
            | otherwise = M.insert nr_key '+' nrMap
        mmmuIL = tail mmuIL
        nr_keys = filter vald $ M.keys nrMap
        vald k = k /= 0
        nr_key = head nr_keys
        no_nr_key = nr_keys == []
        nrMap = pmMap M.! nr
        mmMap
            | nnr /= (-1) = M.insert nnr updt_nnrMap rmMap
            | otherwise = rmMap
        updt_nnrMap
            | no_nnr_key = nnrMap
            | otherwise = M.insert nnr_key '*' nnrMap
        nnr
            | tail mmuIL /= [] = head $ tail mmuIL
            | otherwise = (-1)
        nnr_keys = filter (valid nnrMap) $ M.keys nnrMap
        nnr_key = head nnr_keys
        no_nnr_key = nnr_keys == []
        nnrMap
            | nnr /= (-1) = rmMap M.! nnr
            | otherwise = M.empty

--makeExpressions2 10 [5,6,7,13,17,23,33,41,43,49]
-- *** Exception: Map.!: given key is not an element in the map
-- *Main> makeExpressions2 18 [1,1,1,1,1,1,19,19,19,19,19,1,1,2,2,3,3,14]
--"*** Exception: Map.!: given key is not an element in the map

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
    | t < 9 = makeExpressions t intL
    | otherwise = makeExpressions2 t intL

main :: IO()
main = do
    t <- readLn :: IO Int
    intL <- fmap (map (read :: String -> Int).words) getLine
    putStrLn $ makeExpressions_ t intL

-- 1404
iL = map (read :: String -> Int) $ words "58 72 69 58 98 85 69 88 61 57 40 11 88 32 44 65 14 69 74 49 14 84 40 19 42 93 8 80 14 63 52 28 66 89 66 78 65 28 78 58 4 7 91 77 99 83 25 37 93 17 27 27 34 71 44 77 2 12 92 61 45 6 30 59 82 88 49 82 21 75 90 65 19 50 67 2 70 73 54 22 81 42 65 93 87 90 96 91 74 20 47 82 2 48 74 80 71 38 89 44 90 18 21 53 6 85 13 55 92 78 9 18 24 15 11 75 8 90 61 77 86 41 27 60 54 60 88 9 19 2 46 9 83 20 21 34 73 35 15 57 31 98 94 73 27 24 93 45 15 15 18 17 98 51 85 25 93 30 44 70 90 90 9 88 92 37 63 3 100 56 77 38 45 96 10 54 90 77 97 24 47 68 97 36 62 89 19 49 69 39 78 36 75 65 30 51 2 81 81 21 44 67 3 69 71 43 97 91 29 6 93 20 91 79 51 91 50 36 76 30 54 19 77 88 94 20 19 89 21 34 98 61 78 40 83 78 99 89 30 76 79 68 44 81 38 51 12 47 72 90 54 45 96 14 10 19 8 8 82 50 12 9 55 67 54 99 73 50 46 65 57 86 48 81 84 18 57 13 58 5 99 65 51 67 27 3 52 57 84 32 45 96 18 26 56 10 36 98 13 75 3 26 78 94 41 45 59 12 87 64 16 67 36 30 8 77 6 45 51 95 85 97 32 98 53 43 40 41 42 80 7 22 31 2 16 15 82 13 78 63 75 58 53 83 64 88 78 38 41 56 76 20 83 9 74 27 1 32 80 8 16 74 35 5 41 24 55 11 43 20 81 5 47 53 8 28 19 49 93 72 56 87 73 52 68 5 28 7 36 17 75 52 47 9 41 27 35 75 100 35 20 21 31 99 38 38 59 58 6 18 42 32 31 57 38 68 84 65 64 96 83 14 35 64 28 20 81 89 20 74 83 67 68 40 73 85 63 89 31 41 7 17 50 48 59 16 40 3 4 88 92 56 100 36 20 64 78 28 32 70 32 30 65 12 52 17 35 77 15 31 58 41 67 12 25 19 87 39 21 86 13 62 87 14 59 2 40 12 15 49 67 46 53 58 75 98 25 99 39 32 45 40 90 39 89 89 99 74 87 54 7 76 98 77 66 68 81 24 23 16 59 76 50 33 8 56 100 4 39 69 97 6 71 62 94 62 55 6 54 55 95 72 66 83 73 67 1 78 25 77 1 49 88 23 73 54 92 9 77 56 75 55 71 51 64 61 25 39 22 15 7 58 56 49 16 33 84 22 5 75 67 65 99 93 43 72 27 70 37 22 67 1 50 96 50 81 82 18 94 4 61 100 79 83 98 5 99 86 11 89 42 39 77 76 31 16 48 15 86 24 78 91 24 64 76 14 77 5 83 40 95 50 46 43 20 23 82 30 65 35 3 99 30 96 62 78 94 32 35 83 2 85 18 27 72 74 34 41 56 4 30 80 57 76 47 59 20 53 66 1 88 39 100 42 69 34 29 23 67 83 91 90 100 8 96 98 7 45 20 92 82 97 64 4 15 15 23 19 30 46 86 80 38 20 92 64 85 24 87 98 97 4 58 7 92 17 10 59 88 83 97 60 34 19 35 24 89 31 87 76 8 54 60 61 44 20 20 88 39 76 60 39 43 22 41 34 62 57 81 73 35 26 16 1 16 23 70 100 57 12 78 40 33 51 69 88 67 62 66 67 21 15 73 53 7 33 43 4 89 69 80 1 10 25 35 71 85 97 32 14 58 68 35 6 12 10 14 78 68 51 95 12 59 47 3 98 98 12 18 37 16 57 21 47 12 6 18 2 27 57 85 47 70 77 1 43 54 81 18 41 20 12 13 41 74 70 41 23 9 21 14 98 37 77 26 34 8 6 44 45 41 80 35 89 95 83 85 29 16 32 56 28 84 54 19 31 36 27 2 63 93 45 71 74 42 82 79 66 2 74 63 62 26 22 44 63 67 45 51 82 66 48 36 67 37 12 38 28 8 75 36 14 52 90 89 8 87 24 19 77 91 13 38 94 28 30 70 40 63 48 69 17 63 43 41 40 7 21 31 51 80 96 56 75 47 45 28 49 75 75 6 3 32 54 70 39 60 83 72 90 31 54 73 48 74 70 67 92 12 67 89 93 67 72 74 92 33 10 72 91 19 20 90 63 15 31 92 11 94 30 27 33 52 13 63 33 51 22 53 15 31 68 49 26 78 20 93 75 53 17 1 49 75 53 80 40 83 17 91 86 86 30 36 68 38 86 15 13 68 32 42 52 22 29 96 93 11 22 4 38 83 79 49 92 8 25 34 45 44 23 94 47 31 19 75 12 86 1 88 11 76 81 45 94 90 60 55 59 65 59 34 34 55 86 23 87 86 65 51 38 94 8 11 97 79 99 75 45 48 91 75 64 1 65 71 4 16 36 22 63 92 6 85 27 57 29 37 74 30 40 27 92 7 4 8 80 27 13 54 30 61 88 6 97 65 27 67 65 99 16 55 1 48 52 30 21 41 17 95 76 18 29 12 2 90 67 6 97 95 32 13 1 30 55 17 9 75 78 44 47 4 54 63 48 99 52 42 57 87 77 15 19 73 12 95 65 15 12 63 55 96 76 84 98 93 19 75 84 54 85 69 74 98 18 92 84 93 21 36 63 100 67 34 90 1 71 65 92 18 2 39 60 16 84 82 43 23 10 8 7 52 69 22 67 46 6 64 55 99 36 45 24 8 1 99 40 66 22 9 65 10 37 34 86 66 79 91 88 98 25 40 59 27 100 98 5 72 41 58 58 14 52 80 76 97 77 3 12 81 31 92 87 8 38 8 80 89 38 5 5 30 68 22 13 14 77 26 59 55 64 58 95 54 73 64 53 22 13 41 30 63 32 94 43 99 49 72 30 46 65 83 45 5 36 75 57 95 16 92 9 89 11 63 65 82 68 12 21 19 98 64 78 63 74 1 68 27 28 7 59 94 58 61 53 28 40 91 15 11 69 19 87 13 10 99 87 34 43 71 70 96 88 58 87 33 37 60 82 82 48 89 92 55 37 32 8 90 17 23 75 98 55 87 35 32 25 71 99 87 77 8 59 54 84 88 95 72 35 86 32 1 49 51 17 79 62 8 16 70 13 20 34 17 9"
rL = "58+72+69-58+98+85+69+88+61+57+40+11+88+32+44+65+14+69+74+49+14+84+40+19+42+93+8+80+14+63+52+28+66+89+66+78+65+28+78+58+4+7+91+77+99+83+25+37+93+17+27+27+34+71+44+77+2+12+92+61+45+6+30+59+82+88+49+82+21+75+90+65+19+50+67+2+70+73+54+22+81+42+65+93+87+90+96+91+74+20+47+82+2+48+74+80+71+38+89+44+90+18+21+53+6+85+13+55+92+78+9+18+24+15+11+75+8+90+61+77+86+41+27+60+54+60+88+9+19+2+46+9+83+20+21+34+73+35+15+57+31+98+94+73+27+24+93+45+15+15+18+17+98+51+85+25+93+30+44+70+90+90+9+88+92+37+63+3+100+56+77+38+45+96+10+54+90+77+97+24+47+68+97+36+62+89+19+49+69+39+78+36+75+65+30+51+2+81+81+21+44+67+3+69+71+43+97+91+29+6+93+20+91+79+51+91+50+36+76+30+54+19+77+88+94+20+19+89+21+34+98+61+78+40+83+78+99+89+30+76+79+68+44+81+38+51+12+47+72+90+54+45+96+14+10+19+8+8+82+50+12+9+55+67+54+99+73+50+46+65+57+86+48+81+84+18+57+13+58+5+99+65+51+67+27+3+52+57+84+32+45+96+18+26+56+10+36+98+13+75+3+26+78+94+41+45+59+12+87+64+16+67+36+30+8+77+6+45+51+95+85+97+32+98+53+43+40+41+42+80+7+22+31+2+16+15+82+13+78+63+75+58+53+83+64+88+78+38+41+56+76+20+83+9+74+27+1+32+80+8+16+74+35+5+41+24+55+11+43+20+81+5+47+53+8+28+19+49+93+72+56+87+73+52+68+5+28+7+36+17+75+52+47+9+41+27+35+75+100+35+20+21+31+99+38+38+59+58+6+18+42+32+31+57+38+68+84+65+64+96+83+14+35+64+28+20+81+89+20+74+83+67+68+40+73+85+63+89+31+41+7+17+50+48+59+16+40+3+4+88+92+56+100+36+20+64+78+28+32+70+32+30+65+12+52+17+35+77+15+31+58+41+67+12+25+19+87+39+21+86+13+62+87+14+59+2+40+12+15+49+67+46+53+58+75+98+25+99+39+32+45+40+90+39+89+89+99+74+87+54+7+76+98+77+66+68+81+24+23+16+59+76+50+33+8+56+100+4+39+69+97+6+71+62+94+62+55+6+54+55+95+72+66+83+73+67+1+78+25+77+1+49+88+23+73+54+92+9+77+56+75+55+71+51+64+61+25+39+22+15+7+58+56+49+16+33+84+22+5+75+67+65+99+93+43+72+27+70+37+22+67+1+50+96+50+81+82+18+94+4+61+100+79+83+98+5+99+86+11+89+42+39+77+76+31+16+48+15+86+24+78+91+24+64+76+14+77+5+83+40+95+50+46+43+20+23+82+30+65+35+3+99+30+96+62+78+94+32+35+83+2+85+18+27+72+74+34+41+56+4+30+80+57+76+47+59+20+53+66+1+88+39+100+42+69+34+29+23+67+83+91+90+100+8+96+98+7+45+20+92+82+97+64+4+15+15+23+19+30+46+86+80+38+20+92+64+85+24+87+98+97+4+58+7+92+17+10+59+88+83+97+60+34+19+35+24+89+31+87+76+8+54+60+61+44+20+20+88+39+76+60+39+43+22+41+34+62+57+81+73+35+26+16+1+16+23+70+100+57+12+78+40+33+51+69+88+67+62+66+67+21+15+73+53+7+33+43+4+89+69+80+1+10+25+35+71+85+97+32+14+58+68+35+6+12+10+14+78+68+51+95+12+59+47+3+98+98+12+18+37+16+57+21+47+12+6+18+2+27+57+85+47+70+77+1+43+54+81+18+41+20+12+13+41+74+70+41+23+9+21+14+98+37+77+26+34+8+6+44+45+41+80+35+89+95+83+85+29+16+32+56+28+84+54+19+31+36+27+2+63+93+45+71+74+42+82+79+66+2+74+63+62+26+22+44+63+67+45+51+82+66+48+36+67+37+12+38+28+8+75+36+14+52+90+89+8+87+24+19+77+91+13+38+94+28+30+70+40+63+48+69+17+63+43+41+40+7+21+31+51+80+96+56+75+47+45+28+49+75+75+6+3+32+54+70+39+60+83+72+90+31+54+73+48+74+70+67+92+12+67+89+93+67+72+74+92+33+10+72+91+19+20+90+63+15+31+92+11+94+30+27+33+52+13+63+33+51+22+53+15+31+68+49+26+78+20+93+75+53+17+1+49+75+53+80+40+83+17+91+86+86+30+36+68+38+86+15+13+68+32+42+52+22+29+96+93+11+22+4+38+83+79+49+92+8+25+34+45+44+23+94+47+31+19+75+12+86+1+88+11+76+81+45+94+90+60+55+59+65+59+34+34+55+86+23+87+86+65+51+38+94+8+11+97+79+99+75+45+48+91+75+64+1+65+71+4+16+36+22+63+92+6+85+27+57+29+37+74+30+40+27+92+7+4+8+80+27+13+54+30+61+88+6+97+65+27+67+65+99+16+55+1+48+52+30+21+41+17+95+76+18+29+12+2+90+67+6+97+95+32+13+1+30+55+17+9+75+78+44+47+4+54+63+48+99+52+42+57+87+77+15+19+73+12+95+65+15+12+63+55+96+76+84+98+93+19+75+84+54+85+69+74+98+18+92+84+93+21+36+63+100+67+34+90+1+71+65+92+18+2+39+60+16+84+82+43+23+10+8+7+52+69+22+67+46+6+64+55+99+36+45+24+8+1+99+40+66+22+9+65+10+37+34+86+66+79+91+88+98+25+40+59+27+100+98+5+72+41+58+58+14+52+80+76+97+77+3+12+81+31+92+87+8+38+8+80+89+38+5+5+30+68+22+13+14+77+26+59+55+64+58+95+54+73+64+53+22+13+41+30+63+32+94+43+99+49+72+30+46+65+83+45+5+36+75+57+95+16+92+9+89+11+63+65+82+68+12+21+19+98+64+78+63+74+1+68+27+28+7+59+94+58+61+53+28+40+91+15+11+69+19+87+13+10+99+87+34+43+71+70+96+88+58+87+33+37+60+82+82+48+89+92+55+37+32+8+90+17+23+75+98+55+87+35+32+25+71+99+87+77+8+59+54+84+88+95+72+35+86+32+1+49+51+17+79+62+8+16+70+13+20+34+17+9"
