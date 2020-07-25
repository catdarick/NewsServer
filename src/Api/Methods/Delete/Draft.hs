{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.Draft      as DB
import qualified Database.Checks.User       as DB
import qualified Database.Delete.Draft      as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)

deleteDraft ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteDraft conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, draftId] = requiredValues
  let [] = optionalMaybeValues
  DB.draftAuthorGuard conn (toInt draftId) token
  DB.deleteDraft conn (toInt draftId)
  return (status200, okResponse)
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
