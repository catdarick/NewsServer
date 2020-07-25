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
import           Network.HTTP.Types         (Status, status200)

deleteTag ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteTag conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, tagId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard conn token
  DB.deleteTag conn (toInt tagId)
  return (status200, okResponse)
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
