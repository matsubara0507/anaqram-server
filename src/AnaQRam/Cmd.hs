module AnaQRam.Cmd where

import           RIO

import           AnaQRam.API               (api, server)
import qualified AnaQRam.DB                as DB
import           AnaQRam.Env               (Env)
import qualified Mix.Plugin.Logger         as MixLogger
import qualified Mix.Plugin.Persist.Sqlite as MixDB
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant

app :: RIO Env ()
app = do
  MixLogger.logInfo "Please accsess to localhost:8080"
  unlift <- askUnliftIO
  liftIO $ Warp.run 8080 (appWith unlift)

appWith :: UnliftIO (RIO Env) -> Application
appWith m = serve api $ hoistServer api (liftIO . unliftIO m) server

migrate :: RIO Env ()
migrate = do
  (MixDB.Config config) <- asks (view #sqlite)
  let connName = config ^. #info ^. MixDB.sqlConnectionStr
  MixLogger.logInfo (display $ "Migate SQLite for AnaQRam: " <> connName)
  MixDB.runMigrate DB.migrateAll

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
