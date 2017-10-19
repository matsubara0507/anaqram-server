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
import           Database.Persist
import           Database.Persist.MySQL
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

getConn :: IO ConnectInfo
getConn = do
  let 
    dbName = "MYSQL_"
    info = [ ("HOST", "127.0.0.1")
           , ("PORT", "3306")
           , ("USER", "root")
           , ("PASSWORD", "")
           , ("DATABASE", "anaqram")
           ]
  [host, port, user, pass, db] <-
    mapM (\(k, v) -> fromMaybe v <$> lookupEnv (dbName ++ k)) info
  return $ defaultConnectInfo 
    { connectHost = host
    , connectPort = read port
    , connectUser = user
    , connectPassword = pass
    , connectDatabase = db 
    }

runDB :: ConnectInfo -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB conn = runNoLoggingT . runResourceT . withMySQLConn conn . runSqlConn

doMigration :: IO ()
doMigration = do
  conn <- getConn 
  runNoLoggingT . runResourceT . withMySQLConn conn . runReaderT $ runMigration migrateAll

selectScores :: IO [Score]
selectScores = do
    conn <- getConn
    scoreList <- runDB conn  $ selectList [] []
    return $ map (\(Entity _ u) -> toScore u) scoreList

insertScore :: Score -> IO ()
insertScore score = do
    conn <- getConn
    runDB conn . insert_ . fromScore $ score

