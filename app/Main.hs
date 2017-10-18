{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Configuration.Dotenv      as Dotenv
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson
import           Data.Proxy                (Proxy (..))
import qualified DB
import qualified Network.Wai.Handler.Warp  as Warp
import qualified Score
import           Servant.API               ((:<|>) (..), (:>), Get, Raw)
import           Servant.EDE               (HTML, loadTemplates)
import           Servant.Server            (Server, serve)
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)

main :: IO ()
main = do
  Dotenv.loadFile False "./config/.env"
  _ <- loadTemplates api [] "."
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server

type API = Get '[HTML "index.html"] Object
         :<|> "static" :> Raw
         :<|> Score.CRUD

api :: Proxy API
api = Proxy

server :: Server API
server = index
    :<|> serveDirectoryFileServer "static"
    :<|> getScores
    :<|> postScore
  where
    index = pure mempty
    getScores = liftIO DB.selectScores
    postScore score = liftIO $ DB.insertScore score >> pure score

