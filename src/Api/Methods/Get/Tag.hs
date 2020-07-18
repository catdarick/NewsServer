{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Tag where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Api.Types.Tag
import           Control.Exception                (try)
import           Control.Exception                (SomeException)
import           Crypto.Hash.MD5                  (hash)
import           Data.Aeson                       (encode)
import           Data.ByteString                  (ByteString)
import           Data.List                        (find)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import qualified Database.Get.Tag                 as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import qualified Database.User                    as DB
import           GHC.Exception                    (errorCallException, throw)
import           Network.HTTP.Types.Status

getTags ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Tag])
getTags conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [mbTagId, mbTagsId, mbName, mbLimit, mbOffset] = optionalMaybeValues
      tags <-
        DB.getTags
          conn
          (fromInt <$> mbTagId)
          (fromIntList <$> mbTagsId)
          mbName
          (fromInt <$> mbLimit)
          (fromInt <$> mbOffset)
      return (status200, payloadResponse tags)
  where
    requiredNames = []
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["tag_id", "tags_id", "name", "limit", "offset"]
    optionalChecks = [isInt, isIntList, isNotEmpty, isIntBetween 1 200, isInt]
    optional = (optionalNames, optionalChecks)
