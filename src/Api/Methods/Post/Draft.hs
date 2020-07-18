{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Post.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters

import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Create.Draft            as DB
import qualified Database.Get.Draft               as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status403)

postDraft ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
postDraft conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, draftId] = requiredValues
      let [] = optionalMaybeValues
      mbAuthorToken <- DB.getDraftAuthorToken conn (toInt draftId)
      case mbAuthorToken of
        [] -> return (status400, errorResponse Err.noDraft)
        [Only authorToken] ->
          if authorToken == token
            then DB.publishDraft conn (toInt draftId) >>
                 return (status200, okResponse)
            else return (status403, errorResponse Err.noPerms)
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
