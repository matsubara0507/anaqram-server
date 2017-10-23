{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Score          (CRUD, Score)
import           Servant.Kotlin (Proxy (Proxy), Spec (Spec), defKotlinImports,
                                 generateKotlinForAPI,
                                 generateKotlinForAPIClass,
                                 generateKotlinForDefDataClass, specsToDir)

spec :: Spec
spec = Spec ["org", "iggg", "anaqram"] className $ mconcat
  [ [ defKotlinImports ]
  , generateKotlinForAPIClass className $ mconcat
      [ generateKotlinForDefDataClass (Proxy :: Proxy Score)
      , generateKotlinForAPI (Proxy :: Proxy CRUD)
      ]
  ]
  where
    className = "ScoreAPI"

main :: IO ()
main = specsToDir [spec] "AnaQRam/app/src/main/java"
