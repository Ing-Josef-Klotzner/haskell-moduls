-- 18_2_Monad_bind.hs
module MonadBind where
import Control.Monad (join)

-- join :: Monad m => m (m a) -> m a
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

-- *MonadBind> putStrLn "Hello, " >> putStrLn "World!"
-- or *MonadBind> putStrLn "Hello, " *> putStrLn "World!"
--Hello, 
--World!

-- *MonadBind> getLine >>= putStrLn
--ein satz
--ein satz

-- *MonadBind> join $ putStrLn <$> getLine    -- <$> is fmap
--eine
--eine

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else []

-- *MonadBind> twiceWhenEven [1,2,3,4]
--[4,4,16,16]

--Short Exercise: Either Monad
--Implement the Either Monad.

data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second x) = Second (f x)
instance Monoid a => Applicative (Sum a) where
    pure = Second
    First x <*> First y = First (x `mappend` y)
    First x <*> _ = First x
    _ <*> First x = First x 
    Second x <*> Second y = Second (x y)
instance Monoid a => Monad (Sum a) where
    return = pure
    First x >>= _ = First x
    Second x >>= f = f x

