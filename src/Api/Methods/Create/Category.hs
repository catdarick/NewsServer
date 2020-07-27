{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Category where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Create.Category   as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status201)
import           State.Types

createCategory ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response Idcont)
createCategory queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, name] = requiredValues
  let [parentId] = optionalMaybeValues
  DB.adminGuard token
  id <- DB.addCategory name (toInt <$> parentId)
  Log.info $ "Category '" <> name <> "' successfully created"
  return (status201, idResponse id)
  where
    requiredNames = ["token", "name"]
    requiredChecks = [isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["parent_id"]
    optionalChecks = [isInt]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
