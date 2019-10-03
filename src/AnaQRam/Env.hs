module AnaQRam.Env where

import qualified Data.Yaml       as Y
import           RIO
import qualified RIO.List        as L
import qualified RIO.Map         as Map
import qualified RIO.Text        as T
import qualified RIO.Vector      as V

import           Data.Extensible

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
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
