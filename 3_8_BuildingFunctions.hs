-- 3_8_BuildingFunctions.hs
module BuildingFunctions where

--dropFirst :: Int -> [Char] -> [Char]
dropFirst :: [a] -> [a]
dropFirst x = drop 1 x
sayLoud :: [Char] -> [Char]
sayLoud x = x ++ "!"
retChar5 :: String -> Char
retChar5 x = 
    if length x > 4 then
        x !! 4
    else
        '?'
retDrop9 :: [Char] -> [Char]
retDrop9 x = 
    if length x > 9 then
        drop 9 x
    else
        "?"
thirdLetter :: String -> Char
thirdLetter x = 
    if length x > 2 then
        x !! 2
    else
        '?'

string = "Curry is awesome"
letterIndex :: Int -> Char
letterIndex x = 
    if (x - 1) < length string && x > 0 then 
        string !! (x - 1)
    else
        '?'
rvrs x = drop 9 x ++ drop 5 (take 9 x) ++ take 5 x
main = do
    putStrLn "this function drops first character of string - try it - give me a string: "
    y <- getLine
    putStrLn $ show (dropFirst y) 
    putStrLn "this function says loud all you say - try it - give me a string: "
    y <- getLine
    putStrLn $ show (sayLoud y) 
    putStrLn "this function returns 5th character - try it - give me a string: "
    y <- getLine
    if retChar5 y == '?' && length y < 5 then
        putStrLn "don't fool me - your input is too short - not having 5th position"
    else
        putStrLn $ show (retChar5 y) 
    putStrLn "this function returns 10th character onwards - try it - give me a string: "
    y <- getLine
    if retDrop9 y == "?" && length y < 10 then
        putStrLn "don't be lazy. give me proper string (more than 9 characters)"
    else
        putStrLn $ show (retDrop9 y) 
    putStrLn "this function returns 3rd character - try it - give me a string: "
    y <- getLine
    if thirdLetter y == '?' && length y < 3 then
        putStrLn "Sorry your input is to short to give you third position"
    else
        putStrLn $ show (thirdLetter y) 
    putStrLn "'letterIndex x' function returns xth char of \"Curry is awesome!\" - try it"
    y <- readLn
    if letterIndex y == '?' && y < 1 || y > length string then
        putStrLn $ show string ++ " has letterindex between 1 and 16. your number is out of range!"
    else
        putStrLn $ show (letterIndex y)
    putStrLn $ show (string) ++ " in reverse: " ++ show (rvrs string)
