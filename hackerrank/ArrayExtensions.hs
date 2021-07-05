{-# LANGUAGE ScopedTypeVariables #-}
module ArrayExtensions where
import Control.Exception (SomeException, try, evaluate)
import qualified Data.Array as A
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)

-- safe lookup for array
lookup :: A.Ix i => i -> A.Array i a -> Maybe a
lookup key array = unsafePerformIO $ lookup_ key array

lookup_ :: forall a i. A.Ix i => i -> A.Array i a -> IO (Maybe a)
--lookup'' :: A.Ix i => i -> A.Array i a -> IO (Maybe a)
lookup_ key array = do
    result <- try (evaluate $ array A.! key) :: IO (Either SomeException a)
    case result of
        Left ex  -> return Nothing
        Right val -> return (Just val)

-- equivalent to A.//
insert :: A.Ix i => i -> e -> A.Array i e -> A.Array i e
insert key content array = array A.// [(key, content)]

-- append content with new key (upperBound + 1)
-- return tuple with appended array and new key
append :: (Num i, A.Ix i) => e -> A.Array i e -> (A.Array i e, i)
append content array = (newA, uB)
    where
    newA = A.array (0, uB) ((uB , content) : (A.assocs array))
    uB = (upperBound array) + 1

-- if key exists, content overwrites old content
-- if key does not exist and 
--      if key is next key (upper bound + 1)
--          then create new array with upper bounds + 1 and old array assocs plus new key and content
--          else put out error message, that next key must be upper bound + 1
insertOrAppend :: (A.Ix i, Num i, Show i) => i -> a -> A.Array i a -> A.Array i a
insertOrAppend key content array = insertAppend
    where
    oldContent = lookup key array
--        putStr $ show q
    insertAppend = case oldContent of
        Nothing -> case () of
            _ | key == uB -> A.array (0, uB) 
                    ((key, content) : (A.assocs array))
            _ | otherwise -> error $ "insertOrAppend: next key for append must be "
                ++ show uB
        Just x -> array A.// [(key, content)]
    uB = (upperBound array) + 1

upperBound :: A.Ix i => A.Array i e -> i
upperBound array = snd (A.bounds array)

{-
testcase12 = A.array (0,9) [(0,((0,1),[(0,1),(0,2),(1,1),(1,2)])),(1,((0,0),[(0,0),(1,0)])),(2,((0,3),[(0,3),(1,3)])),(3,((2,0),[(2,0)])),(4,((2,1),[(2,1),(2,2)])),(5,((2,3),[(2,3)])),(6,((3,0),[(3,0)])),(7,((3,1),[(3,1),(3,2)])),(8,((3,3),[(3,3)])),(9::Int,((4::Int,1::Int),[(4::Int,1::Int),(4,2)]))]
-}
