{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)

--import Database.Persist.Postgresql (withPostgresqlConn)
--import Web.Heroku (dbConnParams)
--import Data.Monoid ((<>))

--runDb :: SqlPersist (ResourceT IO) -> IO a
--runDb query = do
--  let connStr =
--        foldr (\(k, v) t ->
--          t <> (encodeUtf8 $ k <> "=" <> v <> " "))
--        "" params
--  runResourceT . withPostGresqlConn connStr $ runSwlConn query


main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html (mconcat [ "<h1>Scotty, ", beam , " me up!</h1>"])
        --or
        -- html $ "<h1>Scotty, " <>  beam <> " me up!</h1>"

-- write into browser address f.e.:
-- http://localhost:3000/beam
-- --> resulting: Scotty, beam me up!    ... in browser
