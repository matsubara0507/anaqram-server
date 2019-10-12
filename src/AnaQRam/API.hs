module AnaQRam.API where

import           RIO
import qualified RIO.Map                     as Map
import qualified RIO.Text                    as T
import qualified RIO.Vector                  as V

import qualified AnaQRam.DB                  as DB
import           AnaQRam.Env                 (Env)
import qualified AnaQRam.Env                 as AnaQRam
import           AnaQRam.Score               (Score)
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

import           Orphans                     ()

type API
      = Get '[HTML] H.Html
   :<|> "scoreboard" :> Get '[HTML] H.Html
   :<|> "static" :> Raw
   :<|> "api" :> CRUD

type CRUD
      = "sizes"   :> Get '[JSON] [Int]
   :<|> "problem" :> QueryParam' '[Required] "size" Int :> Get '[JSON] String
   :<|> "scores"  :> Get '[JSON] [Score]
   :<|> "scores"  :> ReqBody '[JSON, FormUrlEncoded] Score :> Post '[JSON] Score

api :: Proxy API
api = Proxy

server :: ServerT API (RIO Env)
server = indexHtml
    :<|> scoreboardHtml
    :<|> serveDirectoryFileServer "static"
    :<|> getSizes
    :<|> getProblem
    :<|> getScores
    :<|> postScore

indexHtml :: RIO Env H.Html
indexHtml = do
  Mix.logDebugR "request GET: api=/" nil
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet primerCss
      H.title $ H.text "AnaQRam Web"
    H.div ! H.class_ "Box text-center mt-3 container-sm" $ do
      H.div ! H.class_ "Box-header" $
        H.h1 ! H.class_ "Box-title" $ H.text "AnaQRam Web"
      H.div ! H.class_ "Box-Body" ! H.id "main" $ H.text ""
    forM_ [jsQR, "static/main.js", "static/index.js"] $ \url ->
      H.script ! H.src url $ H.text ""
  where
    jsQR = "https://cdn.jsdelivr.net/npm/jsqr@1.2.0/dist/jsQR.min.js"
    primerCss = "https://cdnjs.cloudflare.com/ajax/libs/Primer/11.0.0/build.css"

scoreboardHtml :: RIO Env H.Html
scoreboardHtml = do
  Mix.logDebugR "request GET: api=/scoreboard" nil
  let jsUrls = ["static/scoreboard/main.js", "static/scoreboard/index.js"]
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet primerCss
      H.title $ H.text "AnaQRam ScoreBoard"
    H.div ! H.id "main" $ H.text ""
    forM_ jsUrls $ \url -> H.script ! H.src url $ H.text ""
  where
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

getScores :: RIO Env [Score]
getScores = do
  Mix.logDebugR "request GET: api=/scores" nil
  DB.selectScores

postScore :: Score -> RIO Env Score
postScore score = do
  Mix.logDebugR "request POST: api=/scores" (#score @= score <: nil)
  DB.insertScore score
  pure score
