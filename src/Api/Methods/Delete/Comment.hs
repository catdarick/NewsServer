{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Comment where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Delete.Comment          as DB
import qualified Database.Get.Comment             as DB
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (status403, Status, status200, status400)

deleteComment ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteComment conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, commentId] = requiredValues
      let [] = optionalMaybeValues
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn token
      commentCreatorId <- DB.getCommentCreator conn (toInt commentId)
      case maybeUserIdAndPriv of
        [] -> return (status400, errorResponse Err.badToken)
        [(_, True)] -> deleteCommentAndReturnRes commentId
        [(userId, _)] ->
          case commentCreatorId of
            [] -> return (status400, errorResponse Err.noComment)
            [Only creatorId] ->
              if creatorId == userId
                then deleteCommentAndReturnRes commentId
                else return (status403, errorResponse Err.noPerms)
  where
    requiredNames = ["token", "comment_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    deleteCommentAndReturnRes commentId = do
      res <- DB.deleteComment conn (toInt commentId)
      case res of
        0 -> return (status400, errorResponse Err.noComment)
        1 -> return (status200, okResponse)
