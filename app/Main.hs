{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.STM    (TVar, atomically, newTVar, readTVar,
                                            writeTVar)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IntMap
import           Data.Proxy                (Proxy (..))
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant.API               ((:<|>) (..), (:>), Get, Raw)
import           Servant.EDE               (HTML, loadTemplates)
import           Servant.Server            (Server, serve)
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)
import           Score                      (Score (..))
import qualified Score

main :: IO ()
main = do
  db <- atomically $ newTVar (length initScoreList, IntMap.fromList initScoreList)
  _ <- loadTemplates api [] "."
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api (server db)

type API = Get '[HTML "index.html"] Object
         :<|> "static" :> Raw
         :<|> Score.CRUD

api :: Proxy API
api = Proxy

server :: TVar (Int, IntMap Score) -> Server API
server db = index
     :<|> serveDirectoryFileServer "static"
     :<|> getScores
     :<|> postScore
  where
    index = pure mempty
    getScores = liftIO $ IntMap.elems . snd <$> atomically (readTVar db)
    postScore score = liftIO . atomically $ do
      (maxId, m) <- readTVar db
      let
        newId = maxId + 1
      writeTVar db (newId, IntMap.insert newId score m)
      pure score

initScoreList :: [(Int, Score)]
initScoreList =
  [ (1, Score 6 10 4)
  , (2, Score 6 12 7)
  , (3, Score 8 15 6)
  ]
