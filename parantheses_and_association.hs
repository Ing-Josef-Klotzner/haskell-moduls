-- Exercises: Parentheses and Association
x = 8 + 7 * 9
y = (8 + 7) * 9
f x = x / 2 + 9
f1 x = x / (2 + 9)
perimeter x y = (x * 2) + (y * 2)
perimeter2 x y = x * 2 + y * 2
main = putStrLn $ "8 + 7 * 9 = " ++ show x ++
            "\n(8 + 7) * 9 = " ++ show y ++
            "\n\n(x * 2) + (y * 2) = perimeter 8 5 = " ++ show (perimeter 8 5) ++
            "\nx * 2 + y * 2 = perimeter2 8 5 = " ++ show (perimeter2 8 5) ++
            "\n\nx / 2 + 9 = f x = " ++ show (f 6) ++
            "\nx / (2 + 9) = f1 x = " ++ show (f1 6)

