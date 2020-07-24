{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Author where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Edit.Author       as DB
import           Database.PostgreSQL.Simple (Connection)

editAuthor :: Connection -> [(ByteString, Maybe Login)] -> IO (Response ())
editAuthor conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, authorId] = requiredValues
  let [description] = optionalMaybeValues
  DB.adminGuard conn token
  DB.editAuthor conn (toInt authorId) description
  return okResponse
  where
    requiredNames = ["token", "author_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["description"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
