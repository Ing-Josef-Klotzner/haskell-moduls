import Data.Monoid
import Control.Monad.Writer

newtype O = O (IO ())

instance Monoid O where 
    mempty = O $ return ()
    mappend (O a) (O b) = O $ a >> b

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

--mergesort :: (Ord a, Show a, MonadWriter O m) => [a] -> m [a]
mergesort [] = return []
mergesort [x] = return [x]
mergesort xs = do
    trace xs 
    let (as, bs) = splitAt (length xs `quot` 2) xs
    liftM2 merge (mergesort as) (mergesort bs)
    where
        trace = tell . O . print

mergesort_ [] = []
mergesort_ [x] = [x]
mergesort_ xs = let (as, bs) = splitAt (length xs `quot` 2) xs
               in merge (mergesort_ as) (mergesort_ bs)

main =
    let (_, O trace) = runWriter $ mergesort [3,1,4,2,10,8,4,7,6]
    in trace
