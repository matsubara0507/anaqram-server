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
import           User                      (User (..))
import qualified User

main :: IO ()
main = do
  db <- atomically $ newTVar (length initUserList, IntMap.fromList initUserList)
  _ <- loadTemplates api [] "."
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api (server db)

type API = Get '[HTML "index.html"] Object
         :<|> "static" :> Raw
         :<|> User.CRUD

api :: Proxy API
api = Proxy

server :: TVar (Int, IntMap User) -> Server API
server db = index
     :<|> serveDirectoryFileServer "static"
     :<|> getUsers
     :<|> postUser
  where
    index = pure mempty
    getUsers = liftIO $ IntMap.elems . snd <$> atomically (readTVar db)
    postUser user = liftIO . atomically $ do
      (maxId, m) <- readTVar db
      let
        newId = maxId + 1
      writeTVar db (newId, IntMap.insert newId user m)
      pure user

initUserList :: [(Int, User)]
initUserList =
  [ (1, User "Alice" 6 10 4)
  , (2, User "Bob"   6 12 7)
  , (3, User "Chris" 8 15 6)
  ]
