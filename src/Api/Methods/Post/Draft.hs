{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Post.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters

import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.Draft            as DB
import qualified Database.Create.Draft            as DB
import qualified Database.Get.Draft               as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status403)

postDraft :: Connection -> [(ByteString, Maybe Login)] -> IO (Response ())
postDraft conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, draftId] = requiredValues
  let [] = optionalMaybeValues
  DB.draftAuthorGuard conn (toInt draftId) token
  DB.publishDraft conn (toInt draftId)
  return okResponse
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
