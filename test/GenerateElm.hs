module Main where

import           RIO

import           AnaQRam.API (CRUD)
import           Servant     ((:>))
import           Servant.Elm (defElmImports, defElmOptions,
                              generateElmModuleWith)

main :: IO ()
main = do
  generateElmModuleWith
    defElmOptions
    ["AnaQRam", "Generated", "API"]
    defElmImports
    "elm-src"
    []
    (Proxy @ ("api" :> CRUD))
