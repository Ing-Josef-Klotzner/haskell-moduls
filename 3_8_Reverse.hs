module Reverse where
string = "Curry is awesome"
rvrs :: String -> String
rvrs x = drop 9 x ++ drop 5 (take 9 x) ++ take 5 x

main :: IO ()
main = print (rvrs string)
