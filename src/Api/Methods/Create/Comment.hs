{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Comment where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.Draft      as DB
import qualified Database.Create.Comment    as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status201)
import           State.Types

createComment ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response Idcont)
createComment queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, newsId, content] = requiredValues
  let [] = optionalMaybeValues
  userId <- DB.getUserId token
  id <- DB.addComment (toInt newsId) userId content
  Log.info $ "Comment for news '" <> newsId <> "' successfully created"
  return (status201, idResponse id)
  where
    requiredNames = ["token", "news_id", "content"]
    requiredChecks = [isNotEmpty, isInt, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
