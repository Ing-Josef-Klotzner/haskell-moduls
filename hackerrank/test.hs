import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V
import           System.Random       (randomRIO)


-- immuutalbe 15 seconds  (compared 3 seconds for mutable)

main :: IO ()
main = do
    let v0 = V.replicate 10 (0 :: Int)

        loop v 0 = return v
        loop v rest = do
            i <- randomRIO (0, 9)
            let oldCount = v ! i
                v' = v // [(i, oldCount + 1)]
            loop v' (rest - 1)

    vector <- loop v0 (10^6)
    print vector
