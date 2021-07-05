module Main where
import Control.Monad (forM_)
import Data.List (isPrefixOf)

isValid uCnt pass login = iValid where
    iValid
        | goV login = go login
        | otherwise = "WRONG PASSWORD"
    goV [] = True
    goV log 
        | passIn log == "" = False 
        | otherwise = True && goV (withoutPass log)
    go [] = ""
    go log 
        | withoutPass log == [] = passIn log ++ (go (withoutPass log))
        | otherwise = passIn log ++ " " ++ (go (withoutPass log))
    passIn lg = go (uCnt - 1) where
        go (-1) = ""
        go uCt 
            | (pass !! uCt) `isPrefixOf` lg = (pass !! uCt) ++ go (- 1)
            | otherwise = go (uCt - 1)
    withoutPass lg = drop (length $ passIn lg) lg

main :: IO()
main = do
    t <- readLn :: IO Int
    forM_ [1..t] (\_ -> do
        userCnt <- readLn :: IO Int
        pass <- fmap words getLine
        login <- getLine :: IO [Char]
        putStrLn $ isValid userCnt pass login -- userCnt ++ " " ++ show pass ++ " " ++ show login
        )
--    putStrLn $ show "3" -- result
