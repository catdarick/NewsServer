{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Comment where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Comment
import           Api.Types.Response
import           Control.Exception                (try)
import           Control.Exception                (SomeException)
import           Crypto.Hash.MD5                  (hash)
import           Data.Aeson                       (encode)
import           Data.ByteString                  (ByteString)
import           Data.List                        (find)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import qualified Database.Get.Comment                as DB
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import           GHC.Exception                    (errorCallException, throw)
import           Network.HTTP.Types.Status

getComments ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Comment])
getComments conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [newsId] = requiredValues
      let [ mbLimit, mbOffset] =optionalMaybeValues
      comments <-
        DB.getComments
          conn
          (fromInt newsId)
          (fromInt <$> mbLimit)
          (fromInt <$> mbOffset)
      return (status200, payloadResponse comments)
  where
    requiredNames = ["news_id"]
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["limit", "offset"]
    optionalChecks = [isIntBetween 1 200, isInt]
    optional = (optionalNames, optionalChecks)
