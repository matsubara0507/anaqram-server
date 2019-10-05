{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mix.Plugin.Persist.Sqlite
  ( HasSqliteConfig (..)
  , Config (..)
  , ConfigR
  , buildPlugin
  , buildPluginWithoutPool
  , run
  , runMigrate
  , Persist.sqlConnectionStr
  , Persist.walEnabled
  , Persist.fkEnabled
  , Persist.extraPragmas
  ) where

import           RIO

import qualified Control.Monad.Logger    as L
import           Data.Extensible
import           Database.Persist.Sqlite (Migration, SqlBackend,
                                          SqliteConnectionInfo)
import qualified Database.Persist.Sqlite as Persist
import           Mix.Plugin              (Plugin, toPlugin)
import           System.Log.FastLogger   (fromLogStr)

newtype Config = Config ConfigR

type ConfigR = Record
  '[ "info" >: SqliteConnectionInfo
   , "pool" >: Int
   ]

buildPlugin :: Text -> Int -> Plugin a m Config
buildPlugin connInfo pookSize = toPlugin $ \f -> f (Config config)
  where
    config = #info @= Persist.mkSqliteConnectionInfo connInfo
          <: #pool @= pookSize
          <: nil

buildPluginWithoutPool :: Text -> Plugin a m Config
buildPluginWithoutPool connInfo = buildPlugin connInfo 0

class HasSqliteConfig env where
  configL :: Lens' env Config

instance Lookup xs "sqlite" Config => HasSqliteConfig (Record xs) where
  configL = lens (view #sqlite) (\x y -> x & #sqlite `set` y)

run ::
  ( MonadUnliftIO m
  , MonadReader env m
  , HasSqliteConfig env
  , HasLogFunc env
  ) => ReaderT SqlBackend m a -> m a
run r = do
  (Config config) <- view configL
  logger <- mkLogger
  flip L.runLoggingT logger $
    if config ^. #pool == 0 then
      (lift . Persist.runSqlConn r) & Persist.withSqliteConnInfo (config ^. #info)
    else
      (lift . Persist.runSqlPool r) & Persist.withSqlitePoolInfo (config ^. #info) (config ^. #pool)

runMigrate ::
  ( MonadUnliftIO m
  , MonadReader env m
  , HasSqliteConfig env
  , HasLogFunc env
  ) => Migration -> m ()
runMigrate = run . Persist.runMigration

mkLogger ::
  ( HasLogFunc env
  , MonadReader env m
  , MonadUnliftIO m
  ) => m (L.Loc -> L.LogSource -> L.LogLevel -> L.LogStr -> IO ())
mkLogger = do
  unliift <- askRunInIO
  pure $ \_ source level msg ->
    unliift $ logGeneric source (toLogLevel level) (displayBytesUtf8 $ fromLogStr msg)
  where
    toLogLevel = \case
      L.LevelDebug     -> LevelDebug
      L.LevelInfo      -> LevelInfo
      L.LevelWarn      -> LevelWarn
      L.LevelError     -> LevelError
      L.LevelOther txt -> LevelOther txt
