{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Author where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Api.Types.Author
import           Control.Exception                (try)
import           Control.Exception                (SomeException)
import           Crypto.Hash.MD5                  (hash)
import           Data.Aeson                       (encode)
import           Data.ByteString                  (ByteString)
import           Data.List                        (find)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import qualified Database.Get.Author                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import qualified Database.User                    as DB
import           GHC.Exception                    (errorCallException, throw)
import           Network.HTTP.Types.Status

getAuthors ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Author])
getAuthors conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [mbAuthorId, mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset] = optionalMaybeValues
      authors <-
        DB.getAuthors
          conn
          (fromInt <$> mbAuthorId)
          (fromInt <$> mbUserId)
          mbLogin
          mbFName
          mbLName
          (fromInt <$> mbLimit)
          (fromInt <$> mbOffset)
      return (status200, payloadResponse authors)
  where
    requiredNames = []
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["author_id", "user_id", "login", "first_name", "last_name", "limit", "offset"]
    optionalChecks =
      [isInt, isInt, isNotEmpty, isNotEmpty, isNotEmpty, isIntBetween 1 200, isInt]
    optional = (optionalNames, optionalChecks)
