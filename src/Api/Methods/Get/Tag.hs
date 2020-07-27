{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Api.Types.Tag
import           Data.ByteString            (ByteString)
import qualified Database.Get.Tag           as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)
import           State.Types
import qualified Logger.Interact            as Log

getTags ::
     [(ByteString, Maybe ByteString)] -> ServerStateIO (Status, Response [Tag])
getTags queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [] = requiredValues
  let [mbTagId, mbTagsId, mbName, mbLimit, mbOffset] = optionalMaybeValues
  tags <-
    DB.getTags
      (toInt <$> mbTagId)
      (toIntList <$> mbTagsId)
      mbName
      (toInt <$> mbLimit)
      (toInt <$> mbOffset)
  return (status200, payloadResponse tags)
  where
    requiredNames = []
    requiredChecks = []
    required = (requiredNames, requiredChecks)
    optionalNames = ["tag_id", "tags_id", "name", "limit", "offset"]
    optionalChecks = [isInt, isIntList, isNotEmpty, isIntBetween 1 200, isInt]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
