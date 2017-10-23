{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Score
    ( Score (..)
    , CRUD
    , crud
    ) where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Proxy                  (Proxy (..))
import           Elm                         (ElmType)
import           GHC.Generics                (Generic)
import           Servant.API                 ((:<|>) (..), (:>), FormUrlEncoded,
                                              Get, JSON, Post, ReqBody)
import           Servant.Kotlin              (KotlinType)
import           Web.Internal.FormUrlEncoded (FromForm)

data Score = Score
  { textLength :: Int
  , clearTime  :: Int
  , swapCount  :: Int
  } deriving (Generic, Show)

instance FromJSON Score
instance ToJSON Score
instance FromForm Score
instance ElmType Score
instance KotlinType Score

type CRUD = "scores" :> Get '[JSON] [Score]
       :<|> "scores" :> ReqBody '[JSON, FormUrlEncoded] Score :> Post '[JSON] Score

crud :: Proxy CRUD
crud = Proxy
