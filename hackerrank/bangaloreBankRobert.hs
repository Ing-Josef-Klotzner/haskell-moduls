import Control.Monad
type Digit = Int
type Time = Int

type Index = Int

-- 1 2 3 4 5 6 7 8 9 0
-- It never makes sense for the two pointers to swap positions. Meaning, the one
-- that is more left will always be more left.
--
-- Argument: For the pointers to swap positions, they have to cross and be on
-- the same field somewhere in between, but from that moment on the two pointers
-- are indistinguishable.
data State = State { _posLeft :: Index -- The position of the left hand finger (can be right though)
                   , _posRight :: Index -- The position of the right hand finger (can be left though)
                   , _timeSpent :: Time
                   }

digitToIndex :: Digit -> Index
digitToIndex 0 = 9
digitToIndex n = n-1

calcTime :: [Digit] -> Time
calcTime digits = minimum . map _timeSpent . map (flip calcTimeImpl indeces) $ makeStates indeces
  where
    indeces = map digitToIndex digits
    makeStates [] = []
    makeStates (i:is)= map (\pR -> State {_posLeft = i, _posRight = pR, _timeSpent = 0})
                       . filter (/=i) $ [0 .. 9]

{--
  1 2 3 4 5 6 7 8 9 0
outer | between | outer
--}
calcTimeImpl :: State -> [Index] -> State
calcTimeImpl st [] = st
calcTimeImpl st (i:is) =
  let
    movesLeft  = i - _posLeft st
    movesRight = i - _posRight st
    absMovesLeft = abs movesLeft
    absMovesRight = abs movesRight
    -- If new position is in outer space (see above) - the cursor that is closer
    -- has to move - see also argument above for not swapping positions.
    isOuter = movesLeft * movesRight >= 0
    newStateOuter = State { _posLeft  = if absMovesLeft < absMovesRight then i else _posLeft st
                          , _posRight = if absMovesRight < absMovesLeft then i else _posRight st
                          , _timeSpent = _timeSpent st + min absMovesLeft absMovesRight + 1
                          }
    leftNewState = st { _posLeft = i
                      , _timeSpent = _timeSpent st + absMovesLeft + 1
                      }
    rightNewState = st { _posRight = i
                       , _timeSpent = _timeSpent st + absMovesRight + 1
                       }
    leftResult = calcTimeImpl leftNewState is
    rightResult = calcTimeImpl rightNewState is
  in
    if isOuter
    then calcTimeImpl newStateOuter is
    else if _timeSpent leftResult < _timeSpent rightResult
         then leftResult
         else rightResult

main :: IO ()
main = do
  void getLine
  digits <- map read . words <$> getLine
  putStrLn . show $ calcTime digits
