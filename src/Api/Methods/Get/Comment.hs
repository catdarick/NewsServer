{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Comment where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Comment
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Get.Comment       as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)
import           State.Types
import qualified Logger.Interact            as Log

getComments ::
     [(ByteString, Maybe ByteString)]
  -> ServerStateIO (Status, Response [Comment])
getComments queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [newsId] = requiredValues
  let [mbLimit, mbOffset] = optionalMaybeValues
  comments <-
    DB.getComments (toInt newsId) (toInt <$> mbLimit) (toInt <$> mbOffset)
  return (status200, payloadResponse comments)
  where
    requiredNames = ["news_id"]
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["limit", "offset"]
    optionalChecks = [isIntBetween 1 200, isInt]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
