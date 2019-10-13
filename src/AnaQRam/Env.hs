module AnaQRam.Env
  ( Env
  , Config
  , readConfig
  , readConfigWith
  , problems
  ) where

import           RIO
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import qualified RIO.Vector                as V

import           AnaQRam.Config.Default    (defaultConfig)
import           AnaQRam.Config.Internal   (Config)
import           Data.Extensible
import qualified Data.Yaml                 as Y
import           Mix.Plugin.Logger         ()
import qualified Mix.Plugin.Persist.Sqlite as MixDB


type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   , "sqlite" >: MixDB.Config
   ]

readConfig :: MonadIO m => FilePath -> m (Either Y.ParseException Config)
readConfig = readConfigWith defaultConfig

readConfigWith ::
  MonadIO m => Config -> FilePath -> m (Either Y.ParseException Config)
readConfigWith def path = do
  file <- readFileBinary path
  pure $ case Y.decodeEither' file of
    Right Y.Null -> Right def
    _            -> hzipWith fromNullable def <$> Y.decodeEither' file

problems :: RIO Env (Map Int (Vector Text))
problems = do
  config <- asks (view #config)
  pure
    $ Map.fromList
    $ map ((,) <$> (maybe 0 T.length . L.headMaybe) <*> V.fromList)
    $ L.groupBy ((==) `on` T.length)
    $ L.sortBy (compare `on` T.length)
    $ config ^. #problems
