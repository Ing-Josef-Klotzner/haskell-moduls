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
diff old new = flip evalState Map.empty $ diff' (zip [0..] old) (zip [0..] new)

testDiff :: [Text] -> [Text] -> (Result, Map (Int, Int) Result)
testDiff old new = flip runState Map.empty $ diff' (zip [0..] old) (zip [0..] new)

diff' :: [Indexed] -> [Indexed] -> State ResultCache Result
diff' [] []           = pure $ Result 0 []
diff' [] new          = pure $ Result (length new) (map (Change Added . snd) new)
diff' old []          = pure $ Result (length old) (map (Change Removed . snd) old)
diff' (o:old) (n:new) = if snd o == snd n
                        then addChange NoChange o <$> diff' old new
                        else getBest o old n new
  where
    getBest :: Indexed -> [Indexed] -> Indexed -> [Indexed] -> State ResultCache Result
    getBest o old n new = do
      cached <- use $ at (fst o, fst n)
      case cached of
        Just r -> pure r
        Nothing -> do
          leftResult  <- addChange Removed o <$> diff' old (n:new)
          rightResult <- addChange Added n   <$> diff' (o:old) new

          let result = if leftResult^.resultChangeCount < rightResult^.resultChangeCount
                       then leftResult
                       else rightResult
          at (fst o, fst n) .= Just result
          pure result

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
