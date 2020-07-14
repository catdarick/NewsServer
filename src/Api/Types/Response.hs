{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveGeneric    #-}

module Api.Types.Response where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.ByteString   (ByteString)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import Data.Text.Encoding (decodeUtf8)

data Response resultType =
  Response
    { responseSuccess :: Bool
    , responseError   :: Maybe Text
    , responseResult  :: Maybe resultType
    }
  deriving (Generic, Eq, Show)

instance (ToJSON resultType) => ToJSON (Response resultType) where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}


defaultResponse success =
  Response 
    { responseSuccess = success
    , responseError = Nothing
    , responseResult = Nothing
    }

errorResponse error = (defaultResponse False)  {responseError = Just $ decodeUtf8 error} 

okResponse = (defaultResponse True)

okResponseWithResult result = (defaultResponse True) {responseResult = Just result}

badResoponse :: Response ()
badResoponse = defaultResponse False
