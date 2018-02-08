-- 9_11_Zipping.hs
module Zipping_9_11 where

zip'' :: [a] -> [b] -> [(a, b)]
zip'' a [] = []
zip'' [] b = []
zip'' a b = [(head a, head b)] ++ zip'' (tail a) (tail b)
--zip'' a b = [((\(x:_) -> x) a, (\(x:_) -> x) b)] ++ zip'' ((\(_:x) -> x) a) ((\(_:x) -> x) b)

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' fun a [] = []
zipWith'' fun [] b = []
zipWith'' fun a b = [fun (head a) (head b)] ++ zipWith'' fun (tail a) (tail b)
