{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module AnaQRam.DB
    ( migrateAll
    , selectScores
    , insertScore
    ) where

import           RIO

import           AnaQRam.Env               (Env)
import           AnaQRam.Score             (Score)
import           Data.Extensible
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Mix.Plugin.Persist.Sqlite as MixDB

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ScoreData
    textLength Int
    clearTime  Int
    swapCount  Int
    deriving Show
|]

toScore :: ScoreData -> Score
toScore (ScoreData a b c)
    = #textLength @= a
   <: #clearTime  @= b
   <: #swapCount  @= c
   <: nil

fromScore :: Score -> ScoreData
fromScore s =
  ScoreData (s ^. #textLength) (s ^. #clearTime) (s ^. #swapCount)

selectScores :: RIO Env [Score]
selectScores = do
    scoreList <- MixDB.run $ selectList [] []
    return $ map (\(Entity _ u) -> toScore u) scoreList

insertScore :: Score -> RIO Env ()
insertScore score =
    MixDB.run $ insert_ (fromScore $ score)
