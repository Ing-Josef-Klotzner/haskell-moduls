module Print3 where
myGreeting :: String
myGreeting = "hello" ++ " world!"
area d = pi * (r * r)
    where r = d / 2
hello :: String
hello = "hello"
world :: String
world = "world!"
main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    putStrLn "area d ... prints area of circle with diameter d (only in ghci available)"
        where secondGreeting = 
                concat [hello, " ", world]

