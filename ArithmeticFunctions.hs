-- Exercises: arithmetic functions
module ArithmeticFunctions where
-- rounds down
dv = div 20 (-6)
-- -4
-- rounds toward zero
qt = quot 20 (-6)
-- -3
weekday = ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]
main = putStrLn $ "\ndiv rounds down:" ++
            "\ndiv 20 (-6) = " ++ show dv ++
            "\nquot rounds toward zero:" ++
            "\nquot 20 (-6) = " ++ show qt ++
            
            "\n\nproof for (quot x y)*y + (rem x y) == x [x:=(-10)] [y:=4]" ++
            "\n(quot (-10) 4) * 4 + (rem (-10) 4) == (-10)" ++
            "\n" ++ show ((quot (-10) 4) * 4 + (rem (-10) 4) == (-10)) ++
            "\nquot (-10) 4 = " ++ show (quot (-10) 4) ++
            "\nrem (-10) 4 = " ++ show (rem (-10) 4) ++
            
            "\n\nproof for (div x y)*y + (mod x y) == x [x:=10] [y:=(-4)]" ++
            "\n(div 10 (-4)) * (-4) + (mod 10 (-4)) == 10" ++
            "\n" ++ show ((div 10 (-4)) * (-4) + (mod 10 (-4)) == 10) ++
            "\ndiv 10 (-4) = " ++ show (div 10 (-4)) ++
            "\nmod 10 (-4) = " ++ show (mod 10 (-4)) ++
            
            "\n\nmod (3 - 12) 7 = " ++ show (mod (3 - 12) 7) ++
            "\nrem (3 - 12) 7 = " ++ show (rem (3 - 12) 7) ++
            "\nweekday = ['sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday']" ++
            "\nif today is wednesday (3), which day was 12 days ago?" ++
            "\nweekday!!(mod (3 - 12) 7)  finds   " ++ show (weekday!!(mod (3 - 12) 7)) ++
            "\nweekday!!(rem (3 - 12) 7 + 7)   finds   " ++ show (weekday!!(rem (3 - 12) 7 + 7)) ++
            
            "\n\n2000 + (negate 1234) = " ++ show (2000 + (negate 1234))

