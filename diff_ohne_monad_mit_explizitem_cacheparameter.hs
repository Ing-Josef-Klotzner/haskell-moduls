{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens hiding (Indexed)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.List
import Data.Monoid
import Data.Hex
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Control.Applicative
import Data.Foldable
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Data.Bifunctor (first)

data ChangeType = NoChange | Added | Removed deriving (Eq)

-- Index items for better performance. (It is faster to compare an Int in a Map than a whole list.)
type Indexed = (Int, Text)

data Change
  = Change { _changeChanged :: ChangeType
           , _changeLine :: Text
           }

data Result
  = Result { _resultChangeCount :: Int
           , _resultChanges :: [Change]
           }

changeTypeToText :: ChangeType -> Text
changeTypeToText NoChange = "="
changeTypeToText Added = "+"
changeTypeToText Removed = "-"

changeToText :: Change -> Text
changeToText (Change type' line) = changeTypeToText type' <> " " <> line

type ResultCache = Map (Int, Int) Result

diff :: [Text] -> [Text] -> Result
diff old new = fst $ diff' Map.empty (zip [0..] old) (zip [0..] new)

diff' :: ResultCache -> [Indexed] -> [Indexed] -> (Result, ResultCache)
diff' c [] []           = (Result 0 [], c) -- No need to cache anything, just pass c on.
diff' c [] new          = (Result (length new) (map (Change Added . snd) new), c) -- No need to cache anything.
diff' c old []          = (Result (length old) (map (Change Removed . snd) old), c) -- No need to cache anything.
diff' c (o:old) (n:new) = if snd o == snd n
                          then first (addChange NoChange o) $ diff' c old new -- Also here, cache is just passed on.
                          else getBest c o old n new
  where
    getBest :: ResultCache -> Indexed -> [Indexed] -> Indexed -> [Indexed] -> (Result, ResultCache)
    getBest c o old n new =
      let
        cached = c ^. at (fst o, fst n) -- Do we have the result already cached?
      in
        case cached of
          Just r -> (r, c) -- Already in cache - we are done! cache won't get modified- just pass it on.
          Nothing ->
            let
              (leftResult, cLeft)   = first (addChange Removed o) $ diff' c     old (n:new)
              (rightResult, cRight) = first (addChange Added n)   $ diff' cLeft (o:old) new -- Right path with result cache from left path

              result = if leftResult^.resultChangeCount < rightResult^.resultChangeCount -- choose best result
                       then leftResult
                       else rightResult
            in
              (result, cRight & at (fst o, fst n) .~ Just result) -- Return best result and upate cache with it.

addChange :: ChangeType -> Indexed -> Result -> Result
addChange cT (_, l) r = r & resultChangeCount %~ (+ numChanges cT)
                          & resultChanges %~ (Change cT l :)
  where
    numChanges NoChange = 0
    numChanges _        = 1


toByteString :: [Text] -> BS.ByteString
toByteString = mconcat . map (fromJust . unhex . T.encodeUtf8 . T.filter (/= ' '))


exampleOld :: [Text]
exampleOld = [ "hallo"
             , "du"
             , "da"
             , "mir"
             , "geht es gut!"
             , "Und dir?"
             ]

exampleNew :: [Text]
exampleNew = [ "hallo"
             , "du"
             , "da"
             , "wie"
             , "geht es dir?"
             , "hallo"
             , "du"
             , "da"
             ]

main :: IO ()
main = do
    -- left  <- T.lines <$> T.readFile "./18_deltas-left"
    -- right <- T.lines <$> T.readFile "./18_deltas"
    [file1, file2] <- getArgs
    left  <- T.lines <$> T.readFile file1
    right <- T.lines <$> T.readFile file2
    let r = diff left right
    let out = T.unlines $ (map changeToText . _resultChanges) r
    T.putStrLn out



instance Show Result where
  show (Result count lines) = unlines $ [ "ChangeCount: " <> show count]
                                        <> map show lines

instance Show Change where
  show (Change t l) = show t <> " " <> T.unpack l


instance Show ChangeType where
  show NoChange   = " "
  show Added      = "+"
  show Removed    = "-"




-- Lenses for Change:

changeChanged :: Lens' Change ChangeType
changeChanged f change' = (\changeChanged' -> change' { _changeChanged = changeChanged' }) <$> f (_changeChanged change')

changeLine :: Lens' Change Text
changeLine f change' = (\changeLine' -> change' { _changeLine = changeLine' }) <$> f (_changeLine change')


-- Lenses for Result:

resultChangeCount :: Lens' Result Int
resultChangeCount f result' = (\resultChangeCount' -> result' { _resultChangeCount = resultChangeCount' }) <$> f (_resultChangeCount result')

resultChanges :: Lens' Result [Change]
resultChanges f result' = (\resultChanges' -> result' { _resultChanges = resultChanges' }) <$> f (_resultChanges result')
