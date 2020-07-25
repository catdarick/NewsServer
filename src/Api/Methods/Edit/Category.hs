{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Category where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Edit.Category     as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)

editCategory ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
editCategory conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, categoryId] = requiredValues
  let [name, parentId] = optionalMaybeValues
  DB.adminGuard conn token
  DB.editCategory conn (toInt categoryId) name (toInt <$> parentId)
  return (status200, okResponse)
  where
    requiredNames = ["token", "category_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["name", "parent_id"]
    optionalChecks = [isNotEmpty, isInt]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
