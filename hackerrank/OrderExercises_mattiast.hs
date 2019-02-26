import qualified Data.Vector.Unboxed as V
import Control.Monad
import Control.Applicative
import Data.List

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine :: IO [Int]
    as <- (map read . words) <$> getLine :: IO [Int]
    let tree = makeTree $ V.fromList $ reverse as
    mapM_ print $ take k $ reverse $ sort $ treeToList tree

-- Return largest contiguous sum in the vector,
-- return starting index and length
maxSumLeft :: V.Vector Int -> Int -> Int -> Maybe (Int, Int, Int)
maxSumLeft xs a b = let
    ys :: V.Vector (Int, Int)
    ys = V.constructN (b - a) $ \ypart -> if V.length ypart == 0
                                            then max (xs V.! a, -1) (0, 0)
                                            else let i = a + V.length ypart
                                                 in max (0,0) (add (xs V.! i, -1) (V.last ypart))
    add (s1, l1) (s2, l2) = (s1+s2, l1+l2)
    mi = b - a - 1 - (V.maxIndex $ V.reverse $ V.map fst ys)
    (maxsum, l) = ys V.! mi
    in if maxsum > 0 then Just (a + mi+l+1, a + mi+1, maxsum)
                     else Nothing

data SumTree = ET | ND Int SumTree SumTree
             deriving Show

makeTree :: V.Vector Int -> SumTree
makeTree vec = let
    go a b | a == b = ET
    go a b = case maxSumLeft vec a b of
                 Just (k, l, s) -> ND s (go a k) (go l b)
                 Nothing -> ET
    in go 0 (V.length vec)

treeToList :: SumTree -> [Int]
treeToList tree = go tree [] where
    go ET acc = acc
    go (ND x left right) acc = x : go right (go left acc)
