{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB
    ( runDB
    , doMigration
    , selectScores
    , insertScore
    ) where

import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.ByteString              ()
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (IsString(fromString))
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Score                        (Score (Score))
import           System.Environment           (lookupEnv)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ScoreData
    textLength Int
    clearTime  Int
    swapCount  Int
    deriving Show
|]

toScore :: ScoreData -> Score
toScore (ScoreData a b c) = Score a b c

fromScore :: Score -> ScoreData
fromScore (Score a b c) = ScoreData a b c

getConn :: String -> ConnectionString
getConn pass =  mconcat 
    [ "host=127.0.0.1", " "
    , "port=5431", " "
    , "sslmode=disable", " "
    , "dbname=postgres", " "
    , "user=postgres", " "
    , "password=", fromString pass
    ]

runDB :: ConnectionString -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB conn = runNoLoggingT . runResourceT . withPostgresqlConn conn . runSqlConn

doMigration :: IO ()
doMigration = do
  conn <- getConn . fromMaybe "postgres" <$> lookupEnv "POSTGRESQL_PASSWORD"
  runNoLoggingT . runResourceT . withPostgresqlConn conn . runReaderT $ runMigration migrateAll

selectScores :: IO [Score]
selectScores = do
    conn <- getConn . fromMaybe "postgres" <$> lookupEnv "POSTGRESQL_PASSWORD"
    scoreList <- runDB conn  $ selectList [] []
    return $ map (\(Entity _ u) -> toScore u) scoreList

insertScore :: Score -> IO ()
insertScore score = do
    conn <- getConn . fromMaybe "postgres" <$> lookupEnv "POSTGRESQL_PASSWORD"
    runDB conn . insert_ . fromScore $ score

