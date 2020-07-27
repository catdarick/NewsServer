{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Edit.Tag          as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

editTag :: [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response ())
editTag queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, tagId] = requiredValues
  let [name] = optionalMaybeValues
  DB.adminGuard token
  DB.editTag (toInt tagId) name
  Log.info $ "Tag '" <> tagId <> "' successfully edited"
  return (status200, okResponse)
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["name"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
