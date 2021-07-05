
eft :: (Ord a, Enum a) => a -> a -> [a]
eft a b = go a b []
    where go c d lst
            | c > d = lst
            | c <= d = go (succ c) d (lst ++ [c])

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []
eftBool False False = [False]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT GT = [LT,EQ,GT]
eftOrd LT EQ = [LT,EQ]
eftOrd EQ GT = [EQ,GT]
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd EQ LT = []
eftOrd LT LT = [LT]
eftOrd GT GT = [GT]
eftOrd EQ EQ = [EQ]

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
