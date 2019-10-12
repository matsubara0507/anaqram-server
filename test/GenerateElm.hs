module Main where

import           RIO

import           AnaQRam.API         (CRUD)
import           AnaQRam.Score       (Score)
import           Elm.Mapping
import           Servant             ((:>))
import           Servant.Elm.Mapping (defElmImports, defElmOptions,
                                      generateElmModuleWith)

main :: IO ()
main = do
  generateElmModuleWith
    defElmOptions
    ["AnaQRam", "Generated", "API"]
    defElmImports
    "elm-src"
    [ DefineElm (Proxy @ Score)
    ]
    (Proxy @ ("api" :> CRUD))
