{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.User where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Delete.User       as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

deleteUser :: [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response ())
deleteUser queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, userId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard token
  DB.deleteUser (toInt userId)
  Log.info $ "User '" <> userId <> "' successfully deleted"
  return (status200, okResponse)
  where
    requiredNames = ["token", "user_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
