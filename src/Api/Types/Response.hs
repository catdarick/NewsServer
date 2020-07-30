{-# LANGUAGE DeriveGeneric #-}

module Api.Types.Response where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics       (Generic)

data Response resultType =
  Response
    { responseSuccess :: Bool
    , responseError   :: Maybe Text
    , responseResult  :: Maybe resultType
    }
  deriving (Generic, Eq, Show)

instance (ToJSON resultType) => ToJSON (Response resultType) where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

newtype Idcont =
  Idcont
    { idcontId :: Int
    }
  deriving (Generic, Eq, Show)

instance ToJSON Idcont where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

idContainer :: Int -> Idcont
idContainer id = Idcont {idcontId = id}

idResponse :: Int -> Response Idcont
idResponse id = okResponse {responseResult = Just $ idContainer id}

defaultResponse :: Bool -> Response resultType
defaultResponse success =
  Response
    { responseSuccess = success
    , responseError = Nothing
    , responseResult = Nothing
    }

errorResponse :: ByteString -> Response ()
errorResponse error =
  (defaultResponse False) {responseError = Just $ decodeUtf8 error}

okResponse :: Response resultType
okResponse = defaultResponse True

okResponseWithResult :: resultType -> Response resultType
okResponseWithResult result =
  (defaultResponse True) {responseResult = Just result}

payloadResponse :: resultType -> Response resultType
payloadResponse payload = (defaultResponse True) {responseResult = Just payload}

badResponse :: Response resultType
badResponse = defaultResponse False
