{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Post.Comment where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.Draft            as DB
import qualified Database.Create.Comment          as DB
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (status200, Status, status400)

postComment ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
postComment conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, newsId, content] = requiredValues
      let [] = optionalMaybeValues
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn token
      case maybeUserIdAndPriv of
        [] -> return (status400, errorResponse Err.badToken)
        [(userId, _)] -> do
          res <- try $ DB.addComment conn (toInt newsId) userId content
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.noNews)
            Right [Only commentId] -> return (status200, idResponse commentId)
  where
    requiredNames = ["token", "news_id", "content"]
    requiredChecks = [isNotEmpty, isInt, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
