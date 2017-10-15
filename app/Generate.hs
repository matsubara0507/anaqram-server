{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy  (Proxy (..))
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmEncoderSource, toElmTypeSource)
import           Servant.Elm (ElmOptions (..), UrlPrefix (Static),
                              defElmImports, defElmOptions,
                              generateElmForAPIWith)
import           Shelly      (run_, shelly)
import           User        (CRUD, User)


elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8080" }

spec :: Spec
spec = Spec ["Generated", "UserAPI"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy User)
             : toElmDecoderSource (Proxy :: Proxy User)
             : toElmEncoderSource (Proxy :: Proxy User)
             : generateElmForAPIWith elmOpts  (Proxy :: Proxy CRUD))

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
