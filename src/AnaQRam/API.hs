module AnaQRam.API where

import           RIO
import qualified RIO.Map                     as Map
import qualified RIO.Text                    as T
import qualified RIO.Vector                  as V

import           AnaQRam.Env                 (Env)
import qualified AnaQRam.Env                 as AnaQRam
import           Data.Extensible
import           Data.Fallible
import           Mix.Plugin.Logger           ()
import qualified Mix.Plugin.Logger.JSON      as Mix
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.StaticFiles  (serveDirectoryFileServer)
import           System.Random
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H hiding (title)

type API
      = Get '[HTML] H.Html
   :<|> "static" :> Raw
   :<|> "api" :> CRUD

type CRUD
      = "sizes" :> Get '[JSON] [Int]
   :<|> "problem" :> QueryParam' '[Required] "size" Int :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: ServerT API (RIO Env)
server = indexHtml "AnaQRam Web"
    :<|> serveDirectoryFileServer "static"
    :<|> getSizes
    :<|> getProblem
  where
    indexHtml title = pure $ H.docTypeHtml $ do
      H.head $ do
        stylesheet primerCss
        H.title $ H.text title
      H.div ! H.class_ "Box text-center mt-3 container-sm" $ do
        H.div ! H.class_ "Box-header" $
          H.h1 ! H.class_ "Box-title" $ H.text title
        H.div ! H.class_ "Box-Body" ! H.id "main" $ H.text ""
      H.script ! H.src "https://cdn.jsdelivr.net/npm/jsqr@1.2.0/dist/jsQR.min.js" $ H.text ""
      H.script ! H.src "static/main.js" $ H.text ""
      H.script ! H.src "static/index.js" $ H.text ""
    primerCss = "https://cdnjs.cloudflare.com/ajax/libs/Primer/11.0.0/build.css"

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"

getSizes :: RIO Env [Int]
getSizes = do
  Mix.logDebugR "request GET: api=/sizes" nil
  problems <- AnaQRam.problems
  pure $ Map.keys problems

getProblem :: Int -> RIO Env String
getProblem wordSize = evalContT $ do
  Mix.logDebugR "request GET: api=/problem" (#size @= wordSize <: nil)
  problems <- lift (Map.lookup wordSize <$> AnaQRam.problems) !?? exit'
  idx <- liftIO $ randomRIO (0, length problems - 1)
  problem <- problems V.!? idx ??? exit'
  pure $ T.unpack problem
  where
    exit' :: ContT String (RIO Env) a
    exit' = do
      Mix.logErrorR "problem not found." (#size @= wordSize <: nil)
      exitA ""
