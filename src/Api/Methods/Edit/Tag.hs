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
import           Network.HTTP.Types         (Status, status200)

editTag :: Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
editTag conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, tagId] = requiredValues
  let [name] = optionalMaybeValues
  DB.adminGuard conn token
  DB.editTag conn (toInt tagId) name
  return (status200, okResponse)
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["name"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
