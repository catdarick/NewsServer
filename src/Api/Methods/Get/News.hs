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

getNews ::
     Connection -> [(ByteString, Maybe ByteString)] -> IO (Response [News])
getNews conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [] = requiredValues
  let [mbAuthorId, mbLogin, mbFName, mbLName, mbCategoryId, mbTagId, mbTagsIdIn, mbTagsIdAll, mbTitle, mbContent, mbLimit, mbOffset, mbSort, mbDate, mbAfterDate, mbBeforeDate] =
        optionalMaybeValues
  news <-
    DB.getNews
      conn
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
  return $ payloadResponse news
  where
    requiredNames = []
    requiredChecks = []
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
    parameters = checkAndGetParameters required optional queryString
