module Main where

import Data.List (intercalate)
--
-- Complete the gradingStudents function below.
--
gradingStudents grades = do
    let grading x = if x >= 38 && x `mod` 5 > 2 then x - (x `mod` 5) + 5 else x
    map grading grades
    
readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do

    n <- readLn :: IO Int

    gradesTemp <- readMultipleLinesAsStringArray n
    let grades = map (read :: String -> Int) gradesTemp

    let result = gradingStudents grades

    putStrLn $ intercalate "\n" $ map (\x -> show x) $ result
    
--    sample input

--    4
--    73
--    67
--    38
--    33

--    expected output

--    75
--    67
--    40
--    33
