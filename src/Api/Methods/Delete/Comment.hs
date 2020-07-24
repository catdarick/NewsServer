{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Comment where

import           Api.ErrorException
import qualified Api.Errors                 as Err
import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Data.ByteString            (ByteString)
import qualified Database.Checks.Comment    as DB
import qualified Database.Checks.User       as DB
import qualified Database.Delete.Comment    as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (status403)

deleteComment :: Connection -> [(ByteString, Maybe Login)] -> IO (Response ())
deleteComment conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, commentId] = requiredValues
  let [] = optionalMaybeValues
  isCommentCreator <- DB.isCommentCreator conn (toInt commentId) token
  isAdmin <- DB.isAdminToken conn token
  if isCommentCreator || isAdmin
    then DB.deleteComment conn (toInt commentId)
    else throwM $ ErrorException status403 Err.noPerms
  return okResponse
  where
    requiredNames = ["token", "comment_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
