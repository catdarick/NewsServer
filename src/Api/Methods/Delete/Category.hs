{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Category where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Delete.Category   as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)

deleteCategory ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteCategory conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, categoryId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard conn token
  DB.deleteCategory conn (toInt categoryId)
  return (status200, okResponse)
  where
    requiredNames = ["token", "category_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
