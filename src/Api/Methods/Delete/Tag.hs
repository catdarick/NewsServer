{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Delete.Tag        as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

deleteTag ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response Idcont)
deleteTag queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, tagId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard token
  DB.deleteTag (toInt tagId)
  Log.info $ "Tag '" <> tagId <> "' successfully deleted"
  return (status200, okResponse)
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
