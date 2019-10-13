{-# LANGUAGE TemplateHaskell #-}

module AnaQRam.Config.Default
  ( defaultConfig
  ) where

import           AnaQRam.Config.Internal as X
import           Data.Yaml.TH            (decodeFile)
import           Instances.TH.Lift       ()

defaultConfig :: Config
defaultConfig = $$(decodeFile "./template/.anaqram-server.yaml")
