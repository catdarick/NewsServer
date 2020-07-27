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
import           Network.HTTP.Types         (Status, status200, status403)
import           State.Types
import qualified Logger.Interact            as Log

deleteComment ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response ())
deleteComment queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, commentId] = requiredValues
  let [] = optionalMaybeValues
  isCommentCreator <- DB.isCommentCreator (toInt commentId) token
  isAdmin <- DB.isAdminToken token
  if isCommentCreator || isAdmin
    then DB.deleteComment (toInt commentId)
    else throwM $ ErrorException status403 Err.noPerms
  Log.info $ "Comment '" <> commentId <> "' successfully deleted"
  return (status200, okResponse)
  where
    requiredNames = ["token", "comment_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
