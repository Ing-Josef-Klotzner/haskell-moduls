module Main where

printn n x = go n where
    go 0 = ""
    go y = x ++ "\n" ++ go (y - 1)

main :: IO()
main = do
    n <- readLn :: IO Int

    -- Print "Hello World" on a new line 'n' times.
    let x = "Hello World"
    putStrLn $ printn n x
