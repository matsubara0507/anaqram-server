module AnaQRam.Config.Internal where

import           RIO

import           Data.Extensible

type Config = Record
  '[ "static_path" >: FilePath
   , "sqlite_path" >: FilePath
   , "problems"    >: [Text]
   ]
