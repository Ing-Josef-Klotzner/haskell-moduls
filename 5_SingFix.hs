-- 5_SingFix.hs

module SingFix where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"
singsong z = if z then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"
sing = singsong False
song = singsong True
        
main =
    putStrLn $ "Here are to build two sentenses with songtitles - if you like sing it" ++
            "\nthe greater title is: \n" ++
            show (sing) ++
            "\nthe other song is:\n" ++
            show (song)
-- originally:
--fstString :: [Char] ++ [Char]
--fstString x = x ++ " in the rain"
--sndString :: [Char] -> Char
--sndString x = x ++ " over the rainbow"
--sing = if (x > y) then fstString x or sndString y
--where x = "Singin"
--x = "Somewhere"
