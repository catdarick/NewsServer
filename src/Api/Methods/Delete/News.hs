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
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

deleteNews :: [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response ())
deleteNews queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, newsId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminOrAuthorGuard (toInt newsId) token
  DB.deleteNews (toInt newsId)
  Log.info $ "News '" <> newsId <> "' successfully deleted"
  return (status200, okResponse)
  where
    requiredNames = ["token", "news_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
