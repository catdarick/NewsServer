{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.News where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.Draft      as DB
import qualified Database.Checks.User       as DB
import qualified Database.Delete.News       as DB
import qualified Database.Get.Draft         as DB
import           Database.PostgreSQL.Simple (Connection)

deleteNews :: Connection -> [(ByteString, Maybe Login)] -> IO (Response Idcont)
deleteNews conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, newsId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminOrAuthorGuard conn (toInt newsId) token
  DB.deleteNews conn (toInt newsId)
  return okResponse
  where
    requiredNames = ["token", "news_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
