{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.News where

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
import qualified Database.Get.News                as DB
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import           GHC.Exception                    (errorCallException, throw)
import           Network.HTTP.Types.Status

getNews ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [News])
getNews conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [mbAuthorId, mbLogin, mbFName, mbLName, mbCategoryId, mbTagId, mbTagsIdIn, mbTagsIdAll, mbTitle, mbContent, mbLimit, mbOffset, mbSort, mbDate, mbAfterDate, mbBeforeDate] =
            optionalMaybeValues
      news <-
        DB.getNews
          conn
          (fromInt <$> mbAuthorId)
          mbLogin
          mbFName
          mbLName
          (fromInt <$> mbCategoryId)
          (fromInt <$> mbTagId)
          (fromIntList <$> mbTagsIdIn)
          (fromIntList <$> mbTagsIdAll)
          mbTitle
          mbContent
          (fromInt <$> mbSort)
          (toDate <$> mbDate)
          (toDate <$> mbBeforeDate)
          (toDate <$> mbAfterDate)
          (fromInt <$> mbLimit)
          (fromInt <$> mbOffset)
      return (status200, payloadResponse news)
  where
    requiredNames = []
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames =
      [ "author_id"
      , "login"
      , "first_name"
      , "last_name"
      , "category_id"
      , "tag_id"
      , "tags_id_in"
      , "tags_id_all"
      , "title"
      , "content"
      , "limit"
      , "offset"
      , "sort"
      , "date"
      , "after_date"
      , "before_date"
      ]
    optionalChecks =
      [ isInt
      , isNotEmpty
      , isNotEmpty
      , isNotEmpty
      , isInt
      , isInt
      , isIntList
      , isIntList
      , isNotEmpty
      , isNotEmpty
      , isIntBetween 1 200
      , isInt
      , isIntBetween 1 8
      , isDate
      , isDate
      , isDate
      ]
    optional = (optionalNames, optionalChecks)
