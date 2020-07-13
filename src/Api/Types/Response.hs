{-# LANGUAGE DeriveGeneric #-}

module Api.Types.Response where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import Data.ByteString (ByteString)

data Response =
  Response
    { responseSuccess :: Bool
    , responseError   :: Maybe Text
    }
  deriving (Generic, Eq, Show)

instance ToJSON Response where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

defaultResponse success =
  Response {responseSuccess = success, responseError = Nothing}

errorResponse error =
  (defaultResponse False) {responseSuccess = False, responseError = Just error}

okReponse = (defaultResponse True) {responseSuccess = True}
