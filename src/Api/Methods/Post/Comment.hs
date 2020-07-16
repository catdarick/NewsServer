{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Post.Comment where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Control.Exception                (SomeException)
import           Data.ByteString                  (ByteString)
import qualified Database.Comment                 as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import qualified Database.User                    as DB
import           Network.HTTP.Types               (Status, status400)

postComment ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
postComment conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, newsId, content] = requiredValues
      let [] = optionalMaybeValues
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn (token)
      print maybeUserIdAndPriv
      case maybeUserIdAndPriv of
        [] -> return (status400, errorResponse Err.badToken)
        [(userId, _)] -> do
          res <- DB.addComment conn (fromInt newsId) userId content
          case res of
            Left err -> return (status400, errorResponse err)
            Right [Only commentId] -> return (status400, idResponse commentId)
  where
    requiredNames = ["token", "news_id", "content"]
    requiredChecks = [isNotEmpty, isInt, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
