-- 11_6_Vehicles.hs
module Vehicles where

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYouChancesUnited deriving (Eq, Show)
data Size = Small | Medium | Large deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

-- 1. What is the type of myCar? - here is the answer
myCar :: Vehicle
myCar = Car Mini (Price 14000)
urCar :: Vehicle
urCar = Car Mazda (Price 20000)
clownCar :: Vehicle
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Small

-- 2. Given the following, define the functions:
--isCar :: Vehicle -> Bool
--isCar = undefined
--isPlane :: Vehicle -> Bool
--isPlane = undefined
--areCars :: [Vehicle] -> [Bool]
--areCars = undefined

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

--3. Now we’re going to write a function to tell us the manu-
--facturer of a piece of data:
getManu' :: Vehicle -> Manufacturer
getManu' (Car m _) = m
-- To make this safer I'm going to use Maybe
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _ = Nothing

-- just for fun - get a list of manufacturers
getManus :: [Vehicle] -> [Manufacturer]
getManus = map getManu'

--4. Given that we’re returning the Manufacturer, what will hap-
--pen if you use this on Plane data?
--  *** Exception: 11_6_Vehicles.hs:40:1-21: Non-exhaustive patterns in function getManu

--5. All right. Let’s say you’ve decided to add the size of the
--plane as an argument to the Plane constructor. Add that
--to your datatypes in the appropriate places and change
--your data and functions appropriately.
