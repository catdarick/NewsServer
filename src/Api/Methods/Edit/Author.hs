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
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

editAuthor :: [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response ())
editAuthor queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, authorId] = requiredValues
  let [description] = optionalMaybeValues
  DB.adminGuard token
  DB.editAuthor (toInt authorId) description
  Log.info $ "Author '" <> authorId <> "' successfully edited"
  return (status200, okResponse)
  where
    requiredNames = ["token", "author_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["description"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
