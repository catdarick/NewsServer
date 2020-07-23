{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Comment where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Comment
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Get.Comment             as DB
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status

getComments ::
     Connection -> [(ByteString, Maybe ByteString)] -> IO (Response [Comment])
getComments conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [newsId] = requiredValues
  let [mbLimit, mbOffset] = optionalMaybeValues
  comments <-
    DB.getComments conn (toInt newsId) (toInt <$> mbLimit) (toInt <$> mbOffset)
  return $ payloadResponse comments
  where
    requiredNames = ["news_id"]
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["limit", "offset"]
    optionalChecks = [isIntBetween 1 200, isInt]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
