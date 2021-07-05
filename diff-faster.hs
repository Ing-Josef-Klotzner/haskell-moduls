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

data ChangeType = NoChange | Added | Removed deriving (Eq)

-- Index items for better performance. (It is faster to compare an Int in a Map than a whole list.)
type Indexed = (Int, Text)

data Row = Row { _content :: Text
               , _index :: Int
               , _inOtherFile :: Maybe Int
               }

data Change
  = Change { _changeChanged :: ChangeType
           , _changeLine :: Text
           }

data Result
  = Result { _resultChangeCount :: Int
           , _resultChanges :: [Change]
           }

changeTypeToText :: ChangeType -> Text
changeTypeToText NoChange = " "
changeTypeToText Added = "+"
changeTypeToText Removed = "-"

changeToText :: Change -> Text
changeToText (Change type' line) = changeTypeToText type' <> " " <> line

type ResultCache = Map (Int, Int) Result

diff :: [Text] -> [Text] -> Result
diff old new = flip evalState Map.empty $ diff' oldRows newRows
  where
    oldRows = map (makeRow (Map.fromList newIndexed)) oldIndexed
    newRows = map (makeRow (Map.fromList oldIndexed)) newIndexed

    makeRow :: Map Text Int -> (Text, Int) -> Row
    makeRow other (t, index') = Row t index' (other^.at t)

    oldIndexed = zip old [0..]
    newIndexed = zip new [0..]

-- testDiff :: [Text] -> [Text] -> (Result, Map (Int, Int) Result)
-- testDiff old new = flip runState Map.empty $ diff' (zip [0..] old) (zip [0..] new)

diff' :: [Row] -> [Row] -> State ResultCache Result
diff' [] []           = pure $ Result 0 []
diff' [] new          = pure $ Result (length new) (map (Change Added . _content) new)
diff' old []          = pure $ Result (length old) (map (Change Removed . _content) old)
diff' old'@(o:old) new'@(n:new) = if _content o == _content n
                                  then addChange NoChange o <$> diff' old new
                                  else getBest
  where
    getBest :: State ResultCache Result
    getBest = do
      cached <- use $ at (_index o, _index n)
      case cached of
        Just r -> pure r
        Nothing -> do
          let isOldInNew = inRemainingList o (_index n)
          let isNewInOld = inRemainingList n (_index o)
          result <- case (isOldInNew, isNewInOld) of
                      (False, False) -> addChange Removed o . addChange Added n <$> diff' old new
                      (False, True)  -> addChange Removed o <$> diff' old new'
                      (True, False)  -> addChange Added n <$> diff' old' new
                      (True, True)   -> do
                        leftResult  <- addChange Removed o <$> diff' old new'
                        rightResult <- addChange Added n   <$> diff' old' new

                        pure $ if leftResult^.resultChangeCount < rightResult^.resultChangeCount
                               then leftResult
                               else rightResult
          at (_index o, _index n) .= Just result
          pure result

    inRemainingList :: Row -> Int -> Bool
    inRemainingList (Row _ _ Nothing) _ = False
    inRemainingList (Row _ _ (Just i)) o = i > o

addChange :: ChangeType -> Row -> Result -> Result
addChange cT row r = r & resultChangeCount %~ (+ numChanges cT)
                       & resultChanges %~ (Change cT (_content row) :)
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
    left  <- T.lines <$> T.readFile "./18_deltas-left"
    right <- T.lines <$> T.readFile "./18_deltas"
    let r = diff left right
    BS.writeFile "./same.png"    . toByteString $ getLinesByType NoChange r
    BS.writeFile "./added.png"   . toByteString $ getLinesByType Added r
    BS.writeFile "./removed.png" . toByteString $ getLinesByType Removed r
  where
    getLinesByType :: ChangeType -> Result -> [Text]
    getLinesByType cT = map _changeLine . filter ((== cT) . _changeChanged) . _resultChanges



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
