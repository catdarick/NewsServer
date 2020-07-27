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
import           State.Types
import qualified Logger.Interact            as Log

deleteCategory ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response Idcont)
deleteCategory queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, categoryId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard token
  DB.deleteCategory (toInt categoryId)
  Log.info $ "Category '" <> categoryId <> "' successfully deleted"
  return (status200, okResponse)
  where
    requiredNames = ["token", "category_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
