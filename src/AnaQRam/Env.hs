module AnaQRam.Env where

import           Data.Extensible
import qualified Data.Yaml                 as Y
import           Mix.Plugin.Logger         ()
import qualified Mix.Plugin.Persist.Sqlite as MixDB
import           RIO
import qualified RIO.List                  as L
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as T
import qualified RIO.Vector                as V

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   , "sqlite" >: MixDB.Config
   ]

type Config = Record
  '[ "problems" >: [Text]
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow

problems :: RIO Env (Map Int (Vector Text))
problems = do
  config <- asks (view #config)
  pure
    $ Map.fromList
    $ map ((,) <$> (maybe 0 T.length . L.headMaybe) <*> V.fromList)
    $ L.groupBy ((==) `on` T.length)
    $ L.sortBy (compare `on` T.length)
    $ config ^. #problems
