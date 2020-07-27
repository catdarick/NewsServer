{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.News where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.News
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Get.News          as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

getNews ::
     [(ByteString, Maybe ByteString)] -> ServerStateIO (Status, Response [News])
getNews queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [] = requiredValues
  let [mbNewsId, mbAuthorId, mbLogin, mbFName, mbLName, mbCategoryId, mbTagId, mbTagsIdIn, mbTagsIdAll, mbTitle, mbContent, mbLimit, mbOffset, mbSort, mbDate, mbAfterDate, mbBeforeDate] =
        optionalMaybeValues
  news <-
    DB.getNews
      (toInt <$> mbNewsId)
      (toInt <$> mbAuthorId)
      mbLogin
      mbFName
      mbLName
      (toInt <$> mbCategoryId)
      (toInt <$> mbTagId)
      (toIntList <$> mbTagsIdIn)
      (toIntList <$> mbTagsIdAll)
      mbTitle
      mbContent
      (toInt <$> mbSort)
      (toDate <$> mbDate)
      (toDate <$> mbBeforeDate)
      (toDate <$> mbAfterDate)
      (toInt <$> mbLimit)
      (toInt <$> mbOffset)
  return (status200, payloadResponse news)
  where
    requiredNames = []
    requiredChecks = []
    required = (requiredNames, requiredChecks)
    optionalNames =
      [ "news_id"
      , "author_id"
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
      , isInt
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
    parameters = checkAndGetParameters required optional queryString
