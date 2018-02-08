mySqr = [x^2 | x <- [1..10]]
mySqr2 = take 5 [(x, y) | x <- mySqr,y <- mySqr,x < 50, y > 50]
acro xs = [x | x <- xs, elem x ['A'..'Z']]
myString xs = [x | x <- xs, elem x "aeiou"]
myCube = [y^3 | y <- [1..5]]
myList = [(x,y) | x <- mySqr, x < 50, y <- myCube, y < 50]

main = do
    putStrLn $ "\nmySqr = [x^2 | x <- [1..10]]\n" ++ show mySqr ++ "\n\n" ++
        "mySqr2 = take 5 [(x, y) | x <- mySqr,y <- mySqr,x < 50, y > 50]\n" ++
        show mySqr2 ++ "\n\n" ++
        "acro xs = [x | x <- xs, elem x ['A'..'Z']]\n" ++
        "acro \"Mein Name ist Josef Klotzner\"\n" ++ show (acro "Mein Name ist Josef Klotzner") ++ "\n\n" ++
        "acro \"National Aeronautics and Space Administration\"\n" ++
        show (acro "National Aeronautics and Space Administration") ++ "\n\n" ++
        "acro \"Self Contained Underwater Breathing Apparatus\"\n" ++
        show (acro "Self Contained Underwater Breathing Apparatus") ++ "\n\n" ++
        "myString xs = [x | x <- xs, elem x \"aeiou\"]\n" ++
        "myString \"Mein Name ist Josef Klotzner\"\n" ++
        show (myString "Mein Name ist Josef Klotzner") ++ "\n\n" ++
        "myList = [(x,y) | x <- mySqr, x < 50, y <- myCube, y < 50]\n" ++
        show myList ++
        "\nmyList contains " ++ show (length myList) ++ " tuples"
