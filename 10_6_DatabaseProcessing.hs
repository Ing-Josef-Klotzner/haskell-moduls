-- 10_6_DatabaseProcessing.hs
module DatabaseProcessing where

import Data.Time
data DatabaseItem = DbString String
                | DbNumber Integer
                | DbDate UTCTime
                deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    , DbNumber 9002
    , DbString "Hello, universe!"
    , DbDate (UTCTime (fromGregorian 1931 5 1) (secondsToDiffTime 34123))
    , DbNumber 9003
    , DbString "Hello, multiverse!"
    ]

-- Write a function that filters for DbDate values and returns
-- a list of the UTCTime values inside them.
filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = foldr fil' []
    where fil' a b = case a of
            (DbDate a) -> a : b
            otherwise ->  b

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [t | (DbDate t) <- db]


-- Write a function that filters for DbNumber values and returns
-- a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [n | (DbNumber n) <- db]


-- for own will write function to return DbString values
filterDbStr :: [DatabaseItem] -> [String]
filterDbStr db = [s | (DbString s) <- db]


-- Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate


-- Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber


-- Write a function that gets the average of the DbNumber values.
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb x = (fromIntegral $ sumDb x) / (fromIntegral $ length $ filterDbNumber x)

