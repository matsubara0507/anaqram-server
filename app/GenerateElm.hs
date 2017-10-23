{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy  (Proxy (..))
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmEncoderSource, toElmTypeSource)
import           Score       (CRUD, Score)
import           Servant.Elm (defElmImports, generateElmForAPI)
import           Shelly      (run_, shelly)


spec :: Spec
spec = Spec ["Generated", "ScoreAPI"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Score)
             : toElmDecoderSource (Proxy :: Proxy Score)
             : toElmEncoderSource (Proxy :: Proxy Score)
             : generateElmForAPI (Proxy :: Proxy CRUD))

main :: IO ()
main = do
  specsToDir [spec] "elm-src"
  shelly $
    run_ "elm" ["make", "elm-src/Main.elm", "--output=static/main.js"]
