{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors         as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Edit.Tag          as DB
import           Database.PostgreSQL.Simple (Connection)

editTag :: Connection -> [(ByteString, Maybe Login)] -> IO (Response ())
editTag conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, tagId] = requiredValues
  let [name] = optionalMaybeValues
  DB.adminGuard conn token
  DB.editTag conn (toInt tagId) name
  return okResponse
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["name"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
