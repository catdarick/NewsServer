{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.News
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Get.Draft         as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)

getDrafts ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [News])
getDrafts conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token] = requiredValues
  let [mbCategoryId, mbTagId, mbTagsIdIn, mbTagsIdAll, mbTitle, mbContent, mbLimit, mbOffset] =
        optionalMaybeValues
  news <-
    DB.getDrafts
      conn
      token
      (toInt <$> mbCategoryId)
      (toInt <$> mbTagId)
      (toIntList <$> mbTagsIdIn)
      (toIntList <$> mbTagsIdAll)
      mbTitle
      mbContent
      (toInt <$> mbLimit)
      (toInt <$> mbOffset)
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
    parameters = checkAndGetParameters required optional queryString
