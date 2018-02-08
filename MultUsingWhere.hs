-- multiplication using where
module MultUsingWhere where

mult1 = x * y
    where   x = 5
            y = 6

calc = x * 3 + y
    where   x = 3
            y = 1000
            
calc2 = x * 5
    where   x = 10 * 5 + y
            y = 10

calc3 = z / x + y
    where   x = 7
            y = negate x
            z = y * 10

main = putStrLn $ "\nmult1 = x * y" ++
                "\n   where   x = 5" ++
                "\n           y = 6" ++
                "\nmult1 = " ++ show mult1 ++
                
                "\n\ncalc = x + 3 * y" ++
                "\n   where   x = 3" ++
                "\n           y = 1000" ++
                "\ncalc = " ++ show calc ++
                
                "\n\ncalc2 = x * 5" ++
                "\n   where   x = 10 * 5 + y" ++
                "\n           y = 10" ++
                "\ncalc2 = " ++ show calc2 ++
                
                "\n\ncalc3 = z / x + y" ++
                "\n   where   x = 7" ++
                "\n           y = negate x" ++
                "\n           z = y * 10" ++
                "\n calc3 = " ++ show calc3
