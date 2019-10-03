module AnaQRam.Cmd where

import           RIO

import           AnaQRam.API              (api, server)
import           AnaQRam.Env
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

cmd :: RIO Env ()
cmd = do
  logInfo "Please accsess to localhost:8080"
  unlift <- askUnliftIO
  liftIO $ Warp.run 8080 (appWith unlift)

appWith :: UnliftIO (RIO Env) -> Application
appWith m = serve api $ hoistServer api (liftIO . unliftIO m) server

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
