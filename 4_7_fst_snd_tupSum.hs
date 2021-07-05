-- 4_7_fst_snd_tupSum.hs
module Fst_snd_tupSum where
fst' :: (a, b) -> a
fst' (a, b) = a
snd' :: (a, b) -> b
snd' (a, b) = b
-- Let’s look at another example of pattern matching on tuples:
tupSum :: (Integer, [a])
        -> (Integer, [a])
        -> (Integer, [a])
tupSum (a, b) (c, d) =
    ((a + c), (b ++ d))
    
main = do
    putStrLn $ "would be nice to implement application for fst', snd' and tupSum" ++
            "\nfst' :: (a, b) -> a" ++
            "\nfst' (a, b) = a" ++
            "\nsnd' :: (a, b) -> b" ++
            "\nsnd' (a, b) = b" ++
            "\n\ntupSum :: (Integer, [a]) -> (Integer, [a]) -> (Integer, [a])" ++
            "\ntupSum (a, b) (c, d) = ((a + c), (b ++ d))"
    let a = (38, "wintersport")
    let b = (33, "artikel")
    putStrLn "let a = (38, \"wintersport\")"
    putStrLn "let b = (33, \"artikel\")"
    putStrLn $ "tupSum a b = " ++ show (tupSum a b)
    putStrLn $ "fst' $ tupSum a b = " ++ show (fst' $ tupSum a b)
    putStrLn $ "snd' $ tupSum a b = " ++ show (snd' $ tupSum a b)
