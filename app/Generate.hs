{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy  (Proxy (..))
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmEncoderSource, toElmTypeSource)
import           Servant.Elm (ElmOptions (..), UrlPrefix (Static),
                              defElmImports, defElmOptions,
                              generateElmForAPIWith)
import           Shelly      (run_, shelly)
import           Score        (CRUD, Score)


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8080" }

spec :: Spec
spec = Spec ["Generated", "ScoreAPI"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Score)
             : toElmDecoderSource (Proxy :: Proxy Score)
             : toElmEncoderSource (Proxy :: Proxy Score)
             : generateElmForAPIWith elmOpts  (Proxy :: Proxy CRUD))

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
