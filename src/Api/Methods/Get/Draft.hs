{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Draft where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.News
import           Api.Types.Response
import           Control.Exception                (try)
import           Control.Exception                (SomeException)
import           Crypto.Hash.MD5                  (hash)
import           Data.Aeson                       (encode)
import           Data.ByteString                  (ByteString)
import           Data.List                        (find)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import qualified Database.Get.Draft               as DB
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import           GHC.Exception                    (errorCallException, throw)
import           Network.HTTP.Types.Status

getDrafts ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [News])
getDrafts conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token] = requiredValues
      let [mbCategoryId, mbTagId, mbTagsIdIn, mbTagsIdAll, mbTitle, mbContent, mbLimit, mbOffset] =
            optionalMaybeValues
      news <-
        DB.getDrafts
          conn
          token
          (fromInt <$> mbCategoryId)
          (fromInt <$> mbTagId)
          (fromIntList <$> mbTagsIdIn)
          (fromIntList <$> mbTagsIdAll)
          mbTitle
          mbContent
          (fromInt <$> mbLimit)
          (fromInt <$> mbOffset)
      return (status200, payloadResponse news)
  where
    requiredNames = ["token"]
    requiredChecks = [isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames =
      [ "category_id"
      , "tag_id"
      , "tags_id_in"
      , "tags_id_all"
      , "title"
      , "content"
      , "limit"
      , "offset"
      ]
    optionalChecks =
      [ isInt
      , isInt
      , isIntList
      , isIntList
      , isNotEmpty
      , isNotEmpty
      , isIntBetween 1 200
      , isInt
      ]
    optional = (optionalNames, optionalChecks)
