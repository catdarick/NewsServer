{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.Draft            as DB
import qualified Database.Checks.User             as DB
import qualified Database.Delete.Draft            as DB
import qualified Database.Get.Draft               as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status403, status404)

deleteDraft ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteDraft conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, draftId] = requiredValues
      let [title, content, categoryId, tagsId, mainPicture, pictures] =
            optionalMaybeValues
      mbAuthorToken <- DB.getDraftAuthorToken conn (toInt draftId)
      case mbAuthorToken of
        [] -> return (status400, errorResponse Err.noDraft)
        [Only authorToken] ->
          if authorToken == token
            then do
              DB.deleteDraft conn (toInt draftId)
              return (status200, okResponse)
            else return (status403, errorResponse Err.noPerms)
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames =
      ["title", "content", "category_id", "tags_id", "main_picture", "pictures"]
    optionalChecks =
      [isNotEmpty, isNotEmpty, isInt, isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
