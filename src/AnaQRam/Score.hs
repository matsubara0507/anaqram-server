{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnaQRam.Score
    ( Score
    ) where

import           RIO

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping

type Score = Record
  '[ "textLength" >: Int
   , "clearTime"  >: Int -- ms
   , "swapCount"  >: Int
   ]

instance IsElmType Score where
  compileElmType = compileElmRecordTypeWith "Score"

instance IsElmDefinition Score where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Score"
