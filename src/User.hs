{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module User
    ( User (..)
    , CRUD
    , crud
    ) where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Proxy                  (Proxy (..))
import           Data.Text                   (Text)
import           Elm                         (ElmType)
import           GHC.Generics                (Generic)
import           Servant.API                 ((:<|>) (..), (:>), FormUrlEncoded,
                                              Get, JSON, Post, ReqBody)
import           Web.Internal.FormUrlEncoded (FromForm)

data User = User
  { name       :: Text
  , textLength :: Int
  , clearTime  :: Int
  , swapCount  :: Int
  } deriving (Generic, Show)

instance FromJSON User
instance ToJSON User
instance FromForm User
instance ElmType User

type CRUD = "users" :> Get '[JSON] [User]
       :<|> "users" :> ReqBody '[JSON, FormUrlEncoded] User :> Post '[JSON] User

crud :: Proxy CRUD
crud = Proxy
