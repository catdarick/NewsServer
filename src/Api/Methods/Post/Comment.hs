{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Post.Comment where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.Draft      as DB
import qualified Database.Create.Comment    as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)

postComment :: Connection -> [(ByteString, Maybe Login)] -> IO (Response Idcont)
postComment conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, newsId, content] = requiredValues
  let [] = optionalMaybeValues
  userId <- DB.getUserId conn token
  id <- DB.addComment conn (toInt newsId) userId content
  return $ idResponse id
  where
    requiredNames = ["token", "news_id", "content"]
    requiredChecks = [isNotEmpty, isInt, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
