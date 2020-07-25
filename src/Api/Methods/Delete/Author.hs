{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Author where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Delete.Author     as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)

deleteAuthor ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
deleteAuthor conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, authorId] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard conn token
  DB.deleteAuthor conn (toInt authorId)
  return (status200, okResponse)
  where
    requiredNames = ["token", "author_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
